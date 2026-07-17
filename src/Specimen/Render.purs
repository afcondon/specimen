module Specimen.Render
  ( renderDocument
  , NoteCard
  , Notes
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (regex, replace, test) as Regex
import Data.String.Regex.Flags (global, noFlags) as Regex.Flags

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))

import Specimen.Block (Block(..), Header, ImportLine, ClassBlock, InstanceBlock, ValueBlock, ForeignBlock, DataBlock, TypeAliasBlock, FixityBlock)
import Specimen.Markdown (docLinesToHtml, isCompactMarginalia)
import Specimen.Sig (Connector(..), Station, assignColors, colorize, forallVars, segmentSig, shouldStack)

-- | A single commentary entry. Multiple authors can write commentary on
-- | the same symbol; the renderer stacks each one as a clickable card in
-- | the right margin and emits a corresponding hidden expansion panel
-- | that opens in the middle column.
type NoteCard =
  { authorSlug :: String
  , authorName :: String
  , body       :: String
  }

-- | Margin-note table: symbol-name → array of cards. Keys are written by
-- | the commentary author as `## map`, `## class Functor`,
-- | `## instance functorMaybe`, `## data Proxy`, etc. The renderer
-- | computes the matching key per block, emits a stack of small author
-- | chips in the notes column, and a hidden commentary panel per chip
-- | in the middle column for click-to-expand.
type Notes = Map String (Array NoteCard)

type DocOpts =
  { moduleSlug :: String
  , source     :: String
  , blocks     :: Array Block
  , notes      :: Notes
  }

-- | Render the full document HTML.
renderDocument :: DocOpts -> String
renderDocument { moduleSlug, source, blocks, notes } =
  let
    header = case Array.find isHeader blocks of
      Just (BHeader h) -> renderHeader h
      _                -> renderHeaderFallback moduleSlug
    nonHeader = Array.filter (not <<< isHeader) blocks
    body = renderStack moduleSlug notes nonHeader
    foot = renderFoot { source }
  in
    "<article class=\"specimen-doc\">"
      <> header
      <> body
      <> foot
      <> "</article>"

isHeader :: Block -> Boolean
isHeader (BHeader _) = true
isHeader _           = false

-- ----------------------------------------------------------------------------
-- Header

renderHeader :: Header -> String
renderHeader { name, exports } =
  "<header class=\"specimen-head\">"
    <> "<div class=\"kicker\">PureScript Module</div>"
    <> "<h1>" <> escape name <> "</h1>"
    <> renderExports exports
    <> "</header>"

-- | Render the export list as a cosmetics-label-style grid of bordered
-- | cells, each tagged by kind (Class / Function / Module). Cells are
-- | sorted by kind (classes, then values, then module re-exports) and
-- | the bottom row is padded with empty cells so the grid stays a
-- | complete rectangle.
renderExports :: Array String -> String
renderExports exports =
  if Array.null exports
    then ""
    else
      let
        sorted  = sortByKind (map categorizeExport exports)
        n       = Array.length sorted
        cols    = 4
        padded  = sorted <> Array.replicate (padTo cols n) emptyCell
      in
        "<div class=\"exports\">"
          <> String.joinWith "" (map renderExportCell padded)
          <> "</div>"

padTo :: Int -> Int -> Int
padTo cols n = case n `mod` cols of
  0 -> 0
  r -> cols - r

emptyCell :: { kind :: String, label :: String, name :: String }
emptyCell = { kind: "empty", label: "", name: "" }

sortByKind :: Array { kind :: String, label :: String, name :: String }
           -> Array { kind :: String, label :: String, name :: String }
sortByKind = Array.sortBy (\a b -> compare (kindOrder a.kind) (kindOrder b.kind))

kindOrder :: String -> Int
kindOrder = case _ of
  "class"  -> 0
  "value"  -> 1
  "module" -> 2
  _        -> 3

renderExportCell :: { kind :: String, label :: String, name :: String } -> String
renderExportCell { kind, label, name } =
  "<div class=\"export-cell exp-" <> kind <> "\">"
    <> (if label == ""
          then ""
          else "<span class=\"export-kind\">" <> escape label <> "</span>")
    <> (if name == ""
          then ""
          else "<span class=\"export-name\">" <> softBreaks (escape name) <> "</span>")
    <> "</div>"

categorizeExport :: String -> { kind :: String, label :: String, name :: String }
categorizeExport raw =
  let
    trimmed = String.trim raw
  in case String.stripPrefix (Pattern "class ") trimmed of
    Just rest -> { kind: "class", label: "Class", name: String.trim rest }
    Nothing -> case String.stripPrefix (Pattern "module ") trimmed of
      Just rest -> { kind: "module", label: "Module", name: String.trim rest }
      Nothing -> case String.stripPrefix (Pattern "type ") trimmed of
        Just rest -> { kind: "type", label: "Type", name: String.trim rest }
        Nothing
          | startsUpper trimmed -> { kind: "type", label: "Type", name: trimmed }
          | otherwise           -> { kind: "value", label: "Function", name: trimmed }

startsUpper :: String -> Boolean
startsUpper s = case String.codePointAt 0 s of
  Nothing -> false
  Just _  -> case Regex.regex "^[A-Z]" Regex.Flags.noFlags of
    Right r -> Regex.test r s
    Left _  -> false

renderHeaderFallback :: String -> String
renderHeaderFallback name =
  "<header class=\"specimen-head\">"
    <> "<h1>" <> escape name <> "</h1>"
    <> "</header>"

-- ----------------------------------------------------------------------------
-- Stack — one big subgrid that holds imports + class + instances + values.

renderStack :: String -> Notes -> Array Block -> String
renderStack moduleSlug notes blocks =
  "<div class=\"specimen-stack\">"
    <> String.joinWith "" (map (renderBlock moduleSlug notes) blocks)
    <> "</div>"

renderBlock :: String -> Notes -> Block -> String
renderBlock moduleSlug notes = case _ of
  BHeader _      -> ""  -- already rendered above the stack
  BImports is    -> renderImports is
  BClass    c    -> renderClass    (lookupNote moduleSlug notes (classKey c.head))    c
  BInstance i    -> renderInstance (lookupNote moduleSlug notes (instanceKey i.head)) i
  BForeign  f    -> renderForeign  (lookupNote moduleSlug notes (foreignKey f))       f
  BValue    v    -> renderValue    (lookupNote moduleSlug notes v.name)               v
  BData     d    -> renderData      (lookupNote moduleSlug notes (dataKey d))         d
  BTypeAlias t   -> renderTypeAlias (lookupNote moduleSlug notes ("type " <> t.name)) t
  BFixity   fx   -> renderFixity (lookupNote moduleSlug notes (fixityKey fx)) fx
  BSection  ls   -> renderSection ls
  BRaw      ls   -> renderRaw ls

-- | Compute lookup keys per block kind. Authors write the matching `##`
-- | heading in their commentary markdown.
classKey :: String -> String
classKey head = "class " <> classNameOf head

instanceKey :: String -> String
instanceKey head = case instanceNameOf head of
  Just nm -> "instance " <> nm
  Nothing -> ""    -- anonymous instances aren't keyable; lookup will miss

foreignKey :: ForeignBlock -> String
foreignKey f = (if f.isType then "data " else "") <> f.name

dataKey :: DataBlock -> String
dataKey d = (if d.isNewtype then "newtype " else "data ") <> d.name

-- | Extract the class name from a class declaration head. Handles
-- | constraint contexts (`class Eq a <= Ord a where`) by skipping past
-- | the `<=` or `=>` arrow before grabbing the first identifier.
classNameOf :: String -> String
classNameOf head =
  let
    afterClass = case String.stripPrefix (Pattern "class ") (String.trim head) of
      Just s  -> s
      Nothing -> head
    afterCtx = case String.indexOf (Pattern " <= ") afterClass of
      Just i  -> String.drop (i + 4) afterClass
      Nothing -> case String.indexOf (Pattern " => ") afterClass of
        Just i  -> String.drop (i + 4) afterClass
        Nothing -> afterClass
    trimmed = String.trim afterCtx
  in
    case String.indexOf (Pattern " ") trimmed of
      Just i  -> String.take i trimmed
      Nothing -> trimmed

-- | Named instance? Returns the local name (e.g. "functorMaybe") if the
-- | declaration is `instance NAME :: TYPE where`; Nothing for anonymous
-- | instances (`instance Show (Foo a) where`).
instanceNameOf :: String -> Maybe String
instanceNameOf head = do
  rest <- String.stripPrefix (Pattern "instance ") (String.trim head)
  i    <- String.indexOf (Pattern " :: ") rest
  pure (String.trim (String.take i rest))

-- | Look up note cards for a symbol and render the margin chip stack
-- | plus the hidden commentary panels. Returns "" if no track has
-- | commentary for this symbol.
-- |
-- | Layout: cards stack in the `notes` column on the right; panels
-- | place themselves in the middle column (`name / notes`) and stay
-- | hidden until the corresponding card is clicked. The pairing is
-- | by `data-panel` attribute referencing the panel's id.
lookupNote :: String -> Notes -> String -> String
lookupNote moduleSlug notes key
  | key == "" = ""
  | otherwise = case Map.lookup key notes of
      Nothing    -> ""
      Just cards
        | Array.null cards -> ""
        | otherwise        -> renderNoteCards moduleSlug key cards

renderNoteCards :: String -> String -> Array NoteCard -> String
renderNoteCards moduleSlug key cards =
  let
    indexed = Array.mapWithIndex (\i c -> Tuple (panelId moduleSlug key c.authorSlug i) c) cards
    chips   = String.joinWith "" (map renderNoteChip indexed)
    panels  = String.joinWith "" (map renderNotePanel indexed)
  in
    "<aside class=\"note-stack\">" <> chips <> "</aside>" <> panels

renderNoteChip :: Tuple String NoteCard -> String
renderNoteChip (Tuple pid c) =
  "<button class=\"note-card\" data-panel=\"" <> pid <> "\">"
    <> escape c.authorName
    <> "</button>"

renderNotePanel :: Tuple String NoteCard -> String
renderNotePanel (Tuple pid c) =
  "<div class=\"commentary-panel\" id=\"" <> pid <> "\" hidden>"
    <> "<header class=\"commentary-panel-head\">"
    <> "<span class=\"commentary-by\">Commentary — "
    <> escape c.authorName
    <> "</span>"
    <> "<button class=\"commentary-close\" aria-label=\"Close commentary\">×</button>"
    <> "</header>"
    <> "<div class=\"commentary-body\">"
    <> docLinesToHtml (String.split (Pattern "\n") c.body)
    <> "</div>"
    <> "</div>"

panelId :: String -> String -> String -> Int -> String
panelId moduleSlug key authorSlug i =
  "panel-" <> moduleSlug <> "-" <> sanitizeId key <> "-" <> authorSlug <> "-" <> show i

sanitizeId :: String -> String
sanitizeId =
  String.toLower
    >>> replaceAll (Pattern " ") (Replacement "-")
    >>> replaceAll (Pattern ".") (Replacement "-")
    >>> replaceAll (Pattern ":") (Replacement "-")
    >>> replaceAll (Pattern "(") (Replacement "")
    >>> replaceAll (Pattern ")") (Replacement "")
    >>> replaceAll (Pattern "=") (Replacement "")
    >>> replaceAll (Pattern ">") (Replacement "")
    >>> replaceAll (Pattern "<") (Replacement "")

-- ---- Foreign

renderForeign :: String -> ForeignBlock -> String
renderForeign noteHtml f =
  let label = if f.isType then "Foreign Type" else "Foreign" in
  "<section class=\"row kind-foreign\">"
    <> renderLabel label
    <> "<span class=\"name\">" <> escape f.name <> "</span>"
    <> (if f.sig == ""
          then "<span class=\"sep\"></span><span class=\"type\"></span>"
          else "<span class=\"sep\">::</span>"
            <> "<span class=\"type\">" <> decorateGlyphs (escape f.sig) <> "</span>")
    <> renderMarginalia f.marginalia
    <> noteHtml
    <> "</section>"

-- ---- Fixity — the operator itself takes the name column; the full
-- ---- declaration line sits in the type column with the fixity
-- ---- machinery (`infixr 9`, `as`) quieted to the margin colour, so
-- ---- the eye reads `<<<` … `compose` and the plumbing recedes.

renderFixity :: String -> FixityBlock -> String
renderFixity noteHtml fx =
  "<section class=\"row kind-operator\">"
    <> renderLabel "Operator"
    <> String.joinWith "" (map renderFixityLine fx.fixities)
    <> renderMarginalia fx.marginalia
    <> noteHtml
    <> "</section>"

renderFixityLine :: { alias :: String, code :: String } -> String
renderFixityLine f =
  "<div class=\"fixity-line\">"
    <> "<span class=\"name\">" <> decorateGlyphs (escape f.alias) <> "</span>"
    <> "<span class=\"sep\"></span>"
    <> "<span class=\"type\">" <> decorateGlyphs (quietFixity (escape f.code)) <> "</span>"
    <> "</div>"

fixityKey :: FixityBlock -> String
fixityKey fx = case Array.head fx.fixities of
  Just f  -> "operator " <> f.alias
  Nothing -> ""

-- | Quiet the fixity machinery: `infixr 9` / `infixl 4 type` and the
-- | ` as ` connective drop to the margin colour, leaving the target and
-- | the operator in ink. Applied after `escape`.
quietFixity :: String -> String
quietFixity s =
  case Regex.regex "^(infix[lr]?\\s+\\d+\\s+(?:type\\s+)?)" Regex.Flags.noFlags of
    Right r -> Regex.replace r "<span class=\"kw\">$1</span>"
                 (replaceAll (Pattern " as ") (Replacement "<span class=\"kw\"> as </span>") s)
    Left _  -> s

-- ---- Data / newtype — the lever-frame look: each constructor on its
-- ---- own nested subgrid row, with `=`/`|` landing in the shared
-- ---- [colon] track so the sum's spine aligns with every `::` in the
-- ---- document. Record payloads keep their hand-formatted brace block.

renderData :: String -> DataBlock -> String
renderData noteHtml d =
  let
    label = if d.isNewtype then "Newtype" else "Data"
    kw = if d.isNewtype then "newtype " else "data "
  in
  "<section class=\"row kind-data" <> (if d.isNewtype then " kind-newtype" else "") <> "\">"
    <> renderLabel label
    <> renderKindSig d.kindSig
    <> "<span class=\"name\"><span class=\"kw\">" <> kw <> "</span>" <> escape d.name <> "</span>"
    <> String.joinWith "" (Array.mapWithIndex renderCtorLine d.ctors)
    <> (if Array.null d.payload then "" else renderPreBody d.payload)
    <> renderMarginalia d.marginalia
    <> noteHtml
    <> "</section>"

renderCtorLine :: Int -> { name :: String, args :: String, comment :: Maybe String } -> String
renderCtorLine i ctor =
  "<div class=\"ctor-line\">"
    <> "<span class=\"ctor-sep\">" <> (if i == 0 then "=" else "|") <> "</span>"
    <> "<span class=\"ctor-body\">"
    <> "<span class=\"ctor-name\">" <> escape ctor.name <> "</span>"
    <> (if ctor.args == "" then ""
        else " <span class=\"ctor-args\">" <> decorateGlyphs (escape ctor.args) <> "</span>")
    <> (case ctor.comment of
          Just c  -> "<span class=\"ctor-comment\">" <> escape c <> "</span>"
          Nothing -> "")
    <> "</span>"
    <> "</div>"

-- ---- Type alias — name and `=` in the shared tracks; a multi-line
-- ---- right-hand side (record aliases) keeps its source geometry in a
-- ---- defn-body block.

renderTypeAlias :: String -> TypeAliasBlock -> String
renderTypeAlias noteHtml t =
  "<section class=\"row kind-type\">"
    <> renderLabel "Type"
    <> renderKindSig t.kindSig
    <> "<span class=\"name\"><span class=\"kw\">type </span>" <> escape t.name <> "</span>"
    <> ( case t.rhs of
           [] -> ""
           [ single ] ->
             "<span class=\"sep\">=</span>"
               <> "<span class=\"type\">" <> decorateGlyphs (escape single) <> "</span>"
           lines ->
             "<span class=\"sep\">=</span>"
               <> "<span class=\"type\"></span>"
               <> renderPreBody lines
       )
    <> renderMarginalia t.marginalia
    <> noteHtml
    <> "</section>"

-- | A standalone kind-signature line preceding a data/newtype/type
-- | declaration, typeset through the same name/::/type tracks as every
-- | other signature in the document, with its keyword quieted.
renderKindSig :: Maybe String -> String
renderKindSig = case _ of
  Nothing -> ""
  Just sig -> case String.indexOf (Pattern " :: ") sig of
    Just i ->
      "<div class=\"decl-kind-sig\">"
        <> "<span class=\"name\">" <> quietKeywords (escape (String.take i sig)) <> "</span>"
        <> "<span class=\"sep\">::</span>"
        <> "<span class=\"type\">" <> decorateGlyphs (escape (String.drop (i + 4) sig)) <> "</span>"
        <> "</div>"
    Nothing ->
      "<div class=\"decl-kind-sig\"><span class=\"name\">"
        <> quietKeywords (escape sig) <> "</span></div>"

-- ---- Section headings — an all-comment chunk in the body is the
-- ---- author's own section divider. The decoration lines (runs of
-- ---- ━ ─ - = etc.) are a rule drawn in the only medium a comment
-- ---- allows; we typeset the real rule and set the text as a title.

renderSection :: Array String -> String
renderSection chunk =
  let
    stripComment l =
      let t = String.trim l in
      fromMaybe t (firstJustOf
        [ String.stripPrefix (Pattern "-- |") t
        , String.stripPrefix (Pattern "--") t
        ])
    isRule s = case Regex.regex "^[\\s━─\\-=_*~═▔#]*$" Regex.Flags.noFlags of
      Right r -> Regex.test r s
      Left _  -> false
    titles = Array.filter (\s -> s /= "" && not (isRule s))
      (map (String.trim <<< stripComment) chunk)
  in
    "<section class=\"row kind-section\">"
      <> "<div class=\"section-title\">"
      <> String.joinWith "<br>" (map escape titles)
      <> "</div>"
      <> "</section>"

firstJustOf :: forall a. Array (Maybe a) -> Maybe a
firstJustOf xs = Array.head (Array.catMaybes xs)

-- ---- Imports

renderImports :: Array ImportLine -> String
renderImports lines =
  "<section class=\"row kind-imports\">"
    <> renderLabel "Imports"
    <> "<div class=\"specimen-imports\">"
    <> String.joinWith "" (map renderImportLine lines)
    <> "</div>"
    <> "</section>"

renderImportLine :: ImportLine -> String
renderImportLine { qualified, mod, alias, items } =
  let
    kw     = if qualified then "import qualified" else "import"
    asTxt  = case alias of
      Just a  -> "as " <> a
      Nothing -> ""
    listTxt = case items of
      Just s  -> "(" <> s <> ")"
      Nothing -> ""
  in
       "<span class=\"kw\">"   <> kw       <> "</span>"
    <> "<span class=\"mod\">"  <> escape mod  <> "</span>"
    <> "<span class=\"as\">"   <> escape asTxt <> "</span>"
    <> "<span class=\"list\">" <> escape listTxt <> "</span>"

-- ---- Class

renderClass :: String -> ClassBlock -> String
renderClass noteHtml { head, body, marginalia } =
  "<section class=\"row kind-class\">"
    <> renderLabel "Class"
    <> "<code class=\"class-head\">" <> decorateGlyphs (quietKeywords (escape head)) <> "</code>"
    <> renderClassBody body
    <> renderMarginalia marginalia
    <> noteHtml
    <> "</section>"

-- | Render class-body lines. Each line that parses as a method sig
-- | (`name :: type`) becomes its own nested subgrid row so the `::`
-- | participates in the outer .specimen-stack column tracks. Lines
-- | that don't parse fall back to a pre block (default impls etc.).
renderClassBody :: Array String -> String
renderClassBody body =
  if Array.null body then ""
  else
    let parsed = map classifyClassLine body in
    String.joinWith "" (map renderClassLine parsed)

data ClassLine = ClassMethodSig String String | ClassRaw String

classifyClassLine :: String -> ClassLine
classifyClassLine raw =
  let trimmed = String.trim raw in
  case String.indexOf (Pattern " :: ") trimmed of
    Just i  -> ClassMethodSig (String.take i trimmed)
                              (String.drop (i + 4) trimmed)
    Nothing -> ClassRaw raw

renderClassLine :: ClassLine -> String
renderClassLine = case _ of
  ClassMethodSig name sig ->
    let
      colors = assignColors (forallVars sig)
      typeHtml =
        if shouldStack sig
          then renderStacked colors (segmentSig sig)
          else renderInline  colors sig
    in
      "<div class=\"class-method\">"
        <> "<span class=\"name\">" <> escape name <> "</span>"
        <> "<span class=\"sep\">::</span>"
        <> "<span class=\"type\">" <> typeHtml <> "</span>"
        <> "</div>"
  ClassRaw raw ->
    "<pre class=\"defn-body\">" <> decorateGlyphs (escape raw) <> "</pre>"

-- ---- Instance — split `instance NAME :: TYPE` into name/sep/type so the
-- ---- `::` aligns with value-decl sigs through the shared subgrid.

renderInstance :: String -> InstanceBlock -> String
renderInstance noteHtml { head, body, marginalia } =
  let gathered = gatherInstanceHead head body in
  "<section class=\"row kind-instance\">"
    <> renderLabel "Instance"
    <> renderInstanceSig (parseInstanceHead gathered.headFull)
    <> renderBody gathered.rest
    <> renderMarginalia marginalia
    <> noteHtml
    <> "</section>"

-- | An instance head may continue past its first line: the constraint
-- | block, the target type, and the closing `where` arrive as body
-- | lines. Gather them back into one logical head (whitespace-
-- | normalised, exactly as multi-line function sigs are) so instances
-- | get the same signature typesetting functions do. Anything after
-- | the instance's own `where` is the real body. Nested `where`s in
-- | method bodies can't precede the instance's own, so scanning for
-- | the first is safe.
gatherInstanceHead :: String -> Array String -> { headFull :: String, rest :: Array String }
gatherInstanceHead head body =
  let trimmedHead = String.trim head in
  if endsWithWhere trimmedHead then { headFull: trimmedHead, rest: body }
  else case Array.findIndex (endsWithWhere <<< String.trim) body of
    Just i ->
      { headFull: String.joinWith " " ([ trimmedHead ] <> map String.trim (Array.take (i + 1) body))
      , rest: Array.drop (i + 1) body
      }
    Nothing -> { headFull: trimmedHead, rest: body }

endsWithWhere :: String -> Boolean
endsWithWhere s = s == "where" || case String.stripSuffix (Pattern " where") s of
  Just _  -> true
  Nothing -> false

-- | Typeset an instance head. Named instances split at ` :: ` into the
-- | name column and a signature; anonymous instances put everything
-- | after the (quieted) keyword run through the signature machinery.
-- | The trailing `where` is set at the sig's foot in the head weight —
-- | the same emphasis it gets in a class head.
renderInstanceSig :: { name :: String, sig :: Maybe String } -> String
renderInstanceSig parts =
  let
    sigHtml s =
      let
        stripped = case String.stripSuffix (Pattern " where") (String.trim s) of
          Just s' -> { sig: s', hasWhere: true }
          Nothing -> { sig: String.trim s, hasWhere: false }
        colors = assignColors (forallVars stripped.sig)
        typeHtml =
          if shouldStack stripped.sig
            then renderStacked colors (segmentSig stripped.sig)
            else renderInline colors stripped.sig
      in
        typeHtml <> (if stripped.hasWhere then "<span class=\"inst-where\"> where</span>" else "")
  in case parts.sig of
    Just s ->
      "<span class=\"name\">" <> quietKeywords (escape parts.name) <> "</span>"
        <> "<span class=\"sep\">::</span>"
        <> "<span class=\"type\">" <> sigHtml s <> "</span>"
    Nothing -> case splitLeadingInstanceKeywords parts.name of
      Just { kws, rest } ->
        "<span class=\"name\"><span class=\"kw\">" <> escape kws <> "</span></span>"
          <> "<span class=\"sep\"></span>"
          <> "<span class=\"type\">" <> sigHtml rest <> "</span>"
      Nothing ->
        "<span class=\"name\">" <> quietKeywords (escape parts.name) <> "</span>"
          <> "<span class=\"sep\"></span><span class=\"type\"></span>"

-- | Split the leading keyword run off an anonymous instance head.
-- | Longest candidates first.
splitLeadingInstanceKeywords :: String -> Maybe { kws :: String, rest :: String }
splitLeadingInstanceKeywords s0 =
  Array.head (Array.mapMaybe try candidates)
  where
  s = String.trim s0
  candidates =
    [ "else derive newtype instance "
    , "else derive instance "
    , "else newtype instance "
    , "else instance "
    , "derive newtype instance "
    , "derive instance "
    , "newtype instance "
    , "instance "
    ]
  try p = case String.stripPrefix (Pattern p) s of
    Just rest | rest /= "" -> Just { kws: String.trim p, rest }
    _ -> Nothing

parseInstanceHead :: String -> { name :: String, sig :: Maybe String }
parseInstanceHead h =
  let trimmed = String.trim h in
  case String.indexOf (Pattern " :: ") trimmed of
    Just i -> { name: String.take i trimmed, sig: Just (String.drop (i + 4) trimmed) }
    Nothing -> { name: trimmed, sig: Nothing }

-- ---- Value

renderValue :: String -> ValueBlock -> String
renderValue noteHtml v =
  "<section class=\"row kind-value\">"
    <> renderLabel "Function"
    <> renderSig v.name v.sig
    <> renderBody v.body
    <> renderMarginalia v.marginalia
    <> noteHtml
    <> "</section>"

-- | Emit name + :: + type as direct grid items so the outer .specimen-stack
-- | aligns them vertically across siblings. The type cell either holds an
-- | inline sig (short) or a vertical stack of stations (long), both
-- | colorized against the ∀-bound type variables when present.
renderSig :: String -> Maybe String -> String
renderSig name = case _ of
  Nothing ->
    "<span class=\"name nameonly\">" <> escape name <> "</span>"
      <> "<span class=\"sep\"></span>"
      <> "<span class=\"type\"></span>"
  Just sig ->
    let
      colors = assignColors (forallVars sig)
      typeHtml =
        if shouldStack sig
          then renderStacked colors (segmentSig sig)
          else renderInline  colors sig
    in
      "<span class=\"name\">" <> escape name <> "</span>"
        <> "<span class=\"sep\">::</span>"
        <> "<span class=\"type\">" <> typeHtml <> "</span>"

renderInline :: Map String String -> String -> String
renderInline colors sig =
  decorateGlyphs (colorize colors (escape sig))

-- | Vertical layout: each station emits a body span (left, type-text)
-- | and an op span (right column, holds the trailing →/⇒/.). The two
-- | spans participate as direct items of `.sig-stack`'s auto/auto grid
-- | so the connectors hug the right edge of the longest body rather
-- | than floating at the cell's far right.
renderStacked :: Map String String -> Array Station -> String
renderStacked colors stations0 =
  let
    stations = Array.concatMap splitTupleConstraint stations0
    -- The boundary is the first non-constraint station after at least
    -- one constraint — i.e. the start of the function's actual shape.
    -- Marked so we can give it a small breath of vertical space.
    firstArgIdx = case Array.findLastIndex isConstraint stations of
      Just i -> Just (i + 1)
      Nothing -> Nothing
  in
    "<div class=\"sig-stack\">"
      <> String.joinWith ""
           (Array.mapWithIndex
              (\i s -> renderStation colors (Just i == firstArgIdx) s)
              stations)
      <> "</div>"
  where
  isConstraint s = case s.connector of
    ConConstraint -> true
    _             -> false

-- | A long constraint station whose body is one parenthesized tuple —
-- | the multi-line source style that head-gathering collapsed — breaks
-- | back into one line per constraint. Pure line-breaking: the pieces
-- | concatenate back to the original text.
splitTupleConstraint :: Station -> Array Station
splitTupleConstraint s = case s.connector of
  ConConstraint | String.length s.body > 60 ->
    case oneParenGroup (String.trim s.body) of
      Just inner ->
        let pieces = splitTopLevelComma inner in
        if Array.length pieces < 2 then [ s ]
        else
          Array.mapWithIndex
            (\i p -> { body: (if i == 0 then "( " else ", ") <> p, connector: ConNone })
            pieces
            <> [ { body: ")", connector: ConConstraint } ]
      Nothing -> [ s ]
  _ -> [ s ]

-- | The string with its outer parens removed, provided those parens
-- | enclose the WHOLE string as a single balanced group.
oneParenGroup :: String -> Maybe String
oneParenGroup t = do
  inner0 <- String.stripPrefix (Pattern "(") t
  inner <- String.stripSuffix (Pattern ")") inner0
  let
    depths = Array.scanl
      (\d c -> if c == '(' then d + 1 else if c == ')' then d - 1 else d)
      0
      (SCU.toCharArray inner)
  if Array.all (_ >= 0) depths && Array.last depths /= Just (-1)
    then Just (String.trim inner)
    else Nothing

splitTopLevelComma :: String -> Array String
splitTopLevelComma s0 = go [] [] 0 (SCU.toCharArray s0)
  where
  go acc cur depth xs = case Array.uncons xs of
    Nothing -> Array.snoc acc (String.trim (SCU.fromCharArray cur))
    Just { head: c, tail }
      | c == '(' || c == '{' || c == '[' -> go acc (Array.snoc cur c) (depth + 1) tail
      | c == ')' || c == '}' || c == ']' -> go acc (Array.snoc cur c) (depth - 1) tail
      | c == ',' && depth == 0 ->
          go (Array.snoc acc (String.trim (SCU.fromCharArray cur))) [] 0 tail
      | otherwise -> go acc (Array.snoc cur c) depth tail

-- | Render one station as a body span and an op span. The forall's
-- | trailing `.` is embedded with the binders rather than placed in the
-- | op column — it's binder syntax, not a connector to the next station,
-- | and floating it across an empty gap reads as detached.
-- |
-- | `argsStart` toggles the typographic break between the constraint
-- | section and the function-shape section.
renderStation :: Map String String -> Boolean -> Station -> String
renderStation colors argsStart { body, connector } =
  let
    coloredBody = decorateGlyphs (colorize colors (escape body))
    bodyHtml = case connector of
      ConDot -> coloredBody <> "<small class=\"sig-forall-dot\"> .</small>"
      _      -> coloredBody
    opHtml = case connector of
      ConDot        -> ""
      ConConstraint -> "<span class=\"g\">⇒</span>"
      ConArrow      -> "<span class=\"g\">→</span>"
      ConNone       -> ""
    opClass = case connector of
      ConConstraint -> " sig-op-constraint"
      ConArrow      -> " sig-op-arrow"
      _             -> ""
    boundaryClass = if argsStart then " sig-args-start" else ""
    -- a station carrying a string literal (Warn/Text messages, Fail
    -- constraints) is prose, and prose wraps; type geometry never does
    proseClass = if String.contains (Pattern "\"") body then " sig-station-prose" else ""
  in
    "<span class=\"sig-station-body" <> boundaryClass <> proseClass <> "\">"
      <> bodyHtml
      <> "</span>"
      <> "<span class=\"sig-station-op" <> opClass <> boundaryClass <> "\">"
      <> opHtml
      <> "</span>"

-- | Single-line bodies of the form `name args = expr` render as a
-- | nested subgrid so the leading identifier aligns with the sig's
-- | name above and the `=` lands in the same column as the sig's `::`.
-- | Multi-line bodies (do-blocks, where-clauses, case expressions) keep
-- | the pre fallback because their geometry doesn't fit the 3-col shape.
renderBody :: Array String -> String
renderBody = case _ of
  [] -> ""
  [single] -> case parseSingleLineBody single of
    Just { lhs, rhs } ->
      "<div class=\"value-body\">"
        <> "<span class=\"name\">" <> escape lhs <> "</span>"
        <> "<span class=\"sep\">=</span>"
        <> "<span class=\"type\">" <> decorateGlyphs (escape rhs) <> "</span>"
        <> "</div>"
    Nothing -> renderPreBody [single]
  lines -> renderPreBody lines

renderPreBody :: Array String -> String
renderPreBody lines =
  "<pre class=\"defn-body\">" <> decorateGlyphs (escape (String.joinWith "\n" lines)) <> "</pre>"

-- | Split `name args... = expr` into the lhs and rhs around the first
-- | top-level `=`. Returns Nothing if there's no ` = ` (e.g. a guard
-- | line, or some other body shape we don't yet handle).
parseSingleLineBody :: String -> Maybe { lhs :: String, rhs :: String }
parseSingleLineBody raw =
  let trimmed = String.trim raw in
  case String.indexOf (Pattern " = ") trimmed of
    Just i -> Just
      { lhs: String.trim (String.take i trimmed)
      , rhs: String.trim (String.drop (i + 3) trimmed)
      }
    Nothing -> Nothing

-- ---- Raw fallback

renderRaw :: Array String -> String
renderRaw lines =
  "<section class=\"row kind-raw\">"
    <> "<span class=\"label\"></span>"
    <> "<pre class=\"raw\">"
    <> escape (String.joinWith "\n" lines)
    <> "</pre>"
    <> "</section>"

-- ----------------------------------------------------------------------------
-- Label (left gutter) + Marginalia (full code-area width, below body)

renderLabel :: String -> String
renderLabel label =
  "<span class=\"label\">" <> escape label <> "</span>"

renderMarginalia :: Array String -> String
renderMarginalia notes =
  if Array.null notes
    then ""
    else
      let cls = if isCompactMarginalia notes
                  then "marginalia marginalia-compact"
                  else "marginalia"
      in "<div class=\"" <> cls <> "\">" <> docLinesToHtml notes <> "</div>"

-- ----------------------------------------------------------------------------
-- Foot

renderFoot :: { source :: String } -> String
renderFoot { source } =
  "<footer class=\"specimen-foot\">"
    <> "<div class=\"field\">module from " <> escape source <> "</div>"
    <> "</footer>"

-- ----------------------------------------------------------------------------
-- HTML escaping

escape :: String -> String
escape = replaceAll (Pattern "&")  (Replacement "&amp;")
     >>> replaceAll (Pattern "<")  (Replacement "&lt;")
     >>> replaceAll (Pattern ">")  (Replacement "&gt;")
     >>> replaceAll (Pattern "\"") (Replacement "&quot;")

-- | Insert soft break opportunities at an identifier's morpheme seams —
-- | the lower→Upper camelCase transitions and after underscores — so a
-- | long export name wraps like a hyphenated word (ReadForeign /
-- | Variant), never mid-letter. Applied after `escape`; escape entities
-- | are all-lowercase so the seam regex cannot fire inside them.
softBreaks :: String -> String
softBreaks s = case Regex.regex "([a-z0-9])([A-Z])" Regex.Flags.global of
  Right r -> Regex.replace r "$1<wbr>$2" (replaceAll (Pattern "_") (Replacement "_<wbr>") s)
  Left _  -> s

-- | Quiet the leading declaration keyword(s): the left-gutter label
-- | already names the block kind, so `class` / `instance` in the code
-- | line is typeset in the margin colour — the same emphasis treatment
-- | the imports column gives the `import` keyword. The code text itself
-- | is untouched. Applied after `escape`.
quietKeywords :: String -> String
quietKeywords s =
  case Regex.regex "^((?:else\\s+)?(?:derive\\s+)?(?:newtype\\s+)?(?:class|instance)\\b|(?:newtype|data|type)\\b)" Regex.Flags.noFlags of
    Right r -> Regex.replace r "<span class=\"kw\">$1</span>" s
    Left _  -> s

-- | Wrap each substituted Unicode glyph in `<span class="g">` so CSS can
-- | bump its size to match the visual weight of the JetBrains Mono
-- | ligatures around it. Apply *after* HTML escape; produces HTML.
decorateGlyphs :: String -> String
decorateGlyphs =
  replaceAll (Pattern "→") (Replacement "<span class=\"g\">→</span>")
    >>> replaceAll (Pattern "←") (Replacement "<span class=\"g\">←</span>")
    >>> replaceAll (Pattern "⇒") (Replacement "<span class=\"g\">⇒</span>")
    >>> replaceAll (Pattern "⇐") (Replacement "<span class=\"g\">⇐</span>")
    >>> replaceAll (Pattern "∀") (Replacement "<span class=\"g\">∀</span>")
