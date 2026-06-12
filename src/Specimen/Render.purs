module Specimen.Render
  ( renderDocument
  , NoteCard
  , Notes
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (regex, test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex.Flags

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))

import Specimen.Block (Block(..), Header, ImportLine, ClassBlock, InstanceBlock, ValueBlock, ForeignBlock, DataBlock, TypeAliasBlock)
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
          else "<span class=\"export-name\">" <> escape name <> "</span>")
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

-- ---- Data / newtype — the lever-frame look: each constructor on its
-- ---- own nested subgrid row, with `=`/`|` landing in the shared
-- ---- [colon] track so the sum's spine aligns with every `::` in the
-- ---- document. Record payloads keep their hand-formatted brace block.

renderData :: String -> DataBlock -> String
renderData noteHtml d =
  let label = if d.isNewtype then "Newtype" else "Data" in
  "<section class=\"row kind-data" <> (if d.isNewtype then " kind-newtype" else "") <> "\">"
    <> renderLabel label
    <> "<span class=\"name\">" <> escape d.name <> "</span>"
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
    <> "<span class=\"name\">" <> escape t.name <> "</span>"
    <> ( case t.rhs of
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
    <> "<code class=\"class-head\">" <> decorateGlyphs (escape head) <> "</code>"
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
  let parts = parseInstanceHead head in
  "<section class=\"row kind-instance\">"
    <> renderLabel "Instance"
    <> "<span class=\"name\">" <> escape parts.name <> "</span>"
    <> (case parts.sig of
          Just s ->
            "<span class=\"sep\">::</span>"
              <> "<span class=\"type\">" <> decorateGlyphs (escape s) <> "</span>"
          Nothing ->
            "<span class=\"sep\"></span><span class=\"type\"></span>")
    <> renderBody body
    <> renderMarginalia marginalia
    <> noteHtml
    <> "</section>"

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
renderStacked colors stations =
  let
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
  in
    "<span class=\"sig-station-body" <> boundaryClass <> "\">"
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
