-- | Typeset a classified block stream as a document.
-- |
-- | The renderer builds Halogen HTML, not a `String`. That buys three
-- | things a concatenated document could not have: source text is
-- | escaped once by the vdom rather than by every call site
-- | remembering to; the tree can be embedded directly in a Halogen
-- | component (see `Specimen.Component`); and the same tree serialises
-- | to a static page via `renderDocumentHtml` for the site generator.
-- |
-- | Decoration of code fragments — glyph emphasis, type-variable
-- | colour, quieted keywords — lives in `Specimen.Html` as composable
-- | passes over tokens.
module Specimen.Render
  ( renderDocument
  , renderDocumentHtml
  , DocOpts
  , NoteCard
  , Notes
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))

import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer as StringRenderer

import Specimen.Block (Block(..), ClassBlock, DataBlock, FixityBlock, ForeignBlock, Header, ImportLine, InstanceBlock, RoleLine, TypeAliasBlock, ValueBlock)
import Specimen.Html (Token)
import Specimen.Html as Html
import Specimen.Markdown (docLines, isCompactMarginalia)
import Specimen.Sig (Connector(..), Station, assignColors, forallVars, segmentSig, shouldStack)

-- | A single commentary entry. Multiple authors can write commentary on
-- | the same symbol; the renderer stacks each one as a clickable card in
-- | the right margin and emits a corresponding hidden expansion panel
-- | that opens in the middle column.
type NoteCard =
  { authorSlug :: String
  , authorName :: String
  , body :: String
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
  , source :: String
  , blocks :: Array Block
  , notes :: Notes
  }

-- | Render the full document.
renderDocument :: forall w i. DocOpts -> HH.HTML w i
renderDocument { moduleSlug, source, blocks, notes } =
  HH.article [ cls "specimen-doc" ]
    [ case Array.find isHeader blocks of
        Just (BHeader h) -> renderHeader h
        _ -> renderHeaderFallback moduleSlug
    , HH.div [ cls "specimen-stack" ]
        (Array.concatMap (renderBlock moduleSlug notes)
          (Array.filter (not <<< isHeader) blocks))
    , renderFoot source
    ]

-- | Serialise the document to HTML text, for the static-site generator
-- | and any other consumer that wants a page rather than a component.
renderDocumentHtml :: DocOpts -> String
renderDocumentHtml opts =
  StringRenderer.render absurd (unwrap (renderDocument opts :: HH.PlainHTML))

isHeader :: Block -> Boolean
isHeader = case _ of
  BHeader _ -> true
  _ -> false

-- ----------------------------------------------------------------------------
-- Header

renderHeader :: forall w i. Header -> HH.HTML w i
renderHeader { name, exports, doc } =
  HH.header [ cls "specimen-head" ]
    ( [ HH.div [ cls "kicker" ] [ HH.text "PureScript Module" ]
      , HH.h1_ [ HH.text name ]
      ]
        <> renderExports exports
        <> renderMarginalia doc
    )

renderHeaderFallback :: forall w i. String -> HH.HTML w i
renderHeaderFallback name =
  HH.header [ cls "specimen-head" ] [ HH.h1_ [ HH.text name ] ]

-- | Export kinds, declared in the order the label grid sorts them:
-- | classes first, then values, then module re-exports, then types,
-- | with the padding cells last. The derived `Ord` *is* the sort key.
data ExportKind = ExClass | ExValue | ExModule | ExType | ExEmpty

derive instance Eq ExportKind
derive instance Ord ExportKind

type ExportCell = { kind :: ExportKind, name :: String }

-- | Render the export list as a cosmetics-label-style grid of bordered
-- | cells, each tagged by kind. Cells are sorted by kind and the bottom
-- | row is padded with empty cells so the grid stays a complete
-- | rectangle.
renderExports :: forall w i. Array String -> Array (HH.HTML w i)
renderExports exports
  | Array.null exports = []
  | otherwise =
      let
        sorted = Array.sortWith _.kind (map categorizeExport exports)
        padded = sorted <> Array.replicate (padTo exportColumns (Array.length sorted)) emptyCell
      in
        [ HH.div [ cls "exports" ] (map renderExportCell padded) ]

exportColumns :: Int
exportColumns = 4

padTo :: Int -> Int -> Int
padTo cols n = case n `mod` cols of
  0 -> 0
  r -> cols - r

emptyCell :: ExportCell
emptyCell = { kind: ExEmpty, name: "" }

renderExportCell :: forall w i. ExportCell -> HH.HTML w i
renderExportCell { kind, name } =
  HH.div [ clsx [ "export-cell", "exp-" <> exportSlug kind ] ]
    ( (if kindLabel == "" then [] else [ HH.span [ cls "export-kind" ] [ HH.text kindLabel ] ])
        <> (if name == "" then []
            else [ HH.span [ cls "export-name" ] (Html.toHtml (Html.softBreaks name)) ])
    )
  where
  kindLabel = exportLabel kind

exportSlug :: ExportKind -> String
exportSlug = case _ of
  ExClass -> "class"
  ExValue -> "value"
  ExModule -> "module"
  ExType -> "type"
  ExEmpty -> "empty"

exportLabel :: ExportKind -> String
exportLabel = case _ of
  ExClass -> "Class"
  ExValue -> "Function"
  ExModule -> "Module"
  ExType -> "Type"
  ExEmpty -> ""

categorizeExport :: String -> ExportCell
categorizeExport raw =
  let trimmed = String.trim raw in
  case String.stripPrefix (Pattern "class ") trimmed of
    Just rest -> { kind: ExClass, name: String.trim rest }
    Nothing -> case String.stripPrefix (Pattern "module ") trimmed of
      Just rest -> { kind: ExModule, name: String.trim rest }
      Nothing -> case String.stripPrefix (Pattern "type ") trimmed of
        Just rest -> { kind: ExType, name: String.trim rest }
        Nothing
          | startsUpper trimmed -> { kind: ExType, name: trimmed }
          | otherwise -> { kind: ExValue, name: trimmed }

startsUpper :: String -> Boolean
startsUpper s = case SCU.charAt 0 s of
  Just c -> c >= 'A' && c <= 'Z'
  Nothing -> false

-- ----------------------------------------------------------------------------
-- Block dispatch

renderBlock :: forall w i. String -> Notes -> Block -> Array (HH.HTML w i)
renderBlock moduleSlug notes = case _ of
  BHeader _ -> [] -- already rendered above the stack
  BImports is -> [ renderImports is ]
  BClass c -> [ renderClass (note (classKey c.head)) c ]
  BInstance i -> [ renderInstance (note (instanceKey i.head)) i ]
  BForeign f -> [ renderForeign (note (foreignKey f)) f ]
  BValue v -> [ renderValue (note v.name) v ]
  BData d -> [ renderData (note (dataKey d)) d ]
  BTypeAlias t -> [ renderTypeAlias (note ("type " <> t.name)) t ]
  BFixity fx -> [ renderFixity (note (fixityKey fx)) fx ]
  BRole rs -> [ renderRoles rs ]
  BSection ls -> [ renderSection ls ]
  BRaw ls -> [ renderRaw ls ]
  where
  note = lookupNote moduleSlug notes

-- | Compute lookup keys per block kind. Authors write the matching `##`
-- | heading in their commentary markdown.
classKey :: String -> String
classKey head = "class " <> classNameOf head

instanceKey :: String -> String
instanceKey head = case instanceNameOf head of
  Just nm -> "instance " <> nm
  Nothing -> "" -- anonymous instances aren't keyable; lookup will miss

foreignKey :: ForeignBlock -> String
foreignKey f = (if f.isType then "data " else "") <> f.name

dataKey :: DataBlock -> String
dataKey d = (if d.isNewtype then "newtype " else "data ") <> d.name

fixityKey :: FixityBlock -> String
fixityKey fx = case Array.head fx.fixities of
  Just f -> "operator " <> f.alias
  Nothing -> ""

-- | Extract the class name from a class declaration head. Handles
-- | constraint contexts (`class Eq a <= Ord a where`) by skipping past
-- | the `<=` or `=>` arrow before grabbing the first identifier.
classNameOf :: String -> String
classNameOf head =
  let
    afterClass = fromMaybe head (String.stripPrefix (Pattern "class ") (String.trim head))
    afterCtx = case String.indexOf (Pattern " <= ") afterClass of
      Just i -> String.drop (i + 4) afterClass
      Nothing -> case String.indexOf (Pattern " => ") afterClass of
        Just i -> String.drop (i + 4) afterClass
        Nothing -> afterClass
    trimmed = String.trim afterCtx
  in
    case String.indexOf (Pattern " ") trimmed of
      Just i -> String.take i trimmed
      Nothing -> trimmed

-- | Named instance? Returns the local name (e.g. "functorMaybe") if the
-- | declaration is `instance NAME :: TYPE where`; Nothing for anonymous
-- | instances (`instance Show (Foo a) where`).
instanceNameOf :: String -> Maybe String
instanceNameOf head = do
  rest <- String.stripPrefix (Pattern "instance ") (String.trim head)
  i <- String.indexOf (Pattern " :: ") rest
  pure (String.trim (String.take i rest))

-- ----------------------------------------------------------------------------
-- Commentary

-- | Look up note cards for a symbol and render the margin chip stack
-- | plus the hidden commentary panels. Empty when no track has
-- | commentary for this symbol.
-- |
-- | Layout: cards stack in the `notes` column on the right; panels
-- | place themselves in the middle column (`name / notes`) and stay
-- | hidden until the corresponding card is clicked. The pairing is
-- | by `data-panel` attribute referencing the panel's id.
lookupNote :: forall w i. String -> Notes -> String -> Array (HH.HTML w i)
lookupNote moduleSlug notes key
  | key == "" = []
  | otherwise = case Map.lookup key notes of
      Just cards | not (Array.null cards) -> renderNoteCards moduleSlug key cards
      _ -> []

renderNoteCards :: forall w i. String -> String -> Array NoteCard -> Array (HH.HTML w i)
renderNoteCards moduleSlug key cards =
  let indexed = Array.mapWithIndex (\i c -> Tuple (panelId moduleSlug key c.authorSlug i) c) cards
  in
    [ HH.aside [ cls "note-stack" ] (map renderNoteChip indexed) ]
      <> map renderNotePanel indexed

renderNoteChip :: forall w i. Tuple String NoteCard -> HH.HTML w i
renderNoteChip (Tuple pid c) =
  HH.button
    [ cls "note-card", HP.attr (HH.AttrName "data-panel") pid ]
    [ HH.text c.authorName ]

renderNotePanel :: forall w i. Tuple String NoteCard -> HH.HTML w i
renderNotePanel (Tuple pid c) =
  HH.div [ cls "commentary-panel", HP.id pid, HP.attr (HH.AttrName "hidden") "" ]
    [ HH.header [ cls "commentary-panel-head" ]
        [ HH.span [ cls "commentary-by" ] [ HH.text ("Commentary — " <> c.authorName) ]
        , HH.button
            [ cls "commentary-close", HP.attr (HH.AttrName "aria-label") "Close commentary" ]
            [ HH.text "×" ]
        ]
    , HH.div [ cls "commentary-body" ] (docLines (String.split (Pattern "\n") c.body))
    ]

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

-- ----------------------------------------------------------------------------
-- Foreign

renderForeign :: forall w i. Array (HH.HTML w i) -> ForeignBlock -> HH.HTML w i
renderForeign note f =
  row [ "kind-foreign" ]
    ( [ label (if f.isType then "Foreign Type" else "Foreign")
      , cell "name" [ HH.text f.name ]
      ]
        <> (if f.sig == ""
            then [ cell "sep" [], cell "type" [] ]
            else [ cell "sep" [ HH.text "::" ], cell "type" (glyphed f.sig) ])
        <> renderMarginalia f.marginalia
        <> note
    )

-- ----------------------------------------------------------------------------
-- Fixity — the operator itself takes the name column; the full
-- declaration line sits in the type column with the fixity machinery
-- (`infixr 9`, `as`) quieted to the margin colour, so the eye reads
-- `<<<` … `compose` and the plumbing recedes.

renderFixity :: forall w i. Array (HH.HTML w i) -> FixityBlock -> HH.HTML w i
renderFixity note fx =
  row [ "kind-operator" ]
    ( [ label "Operator" ]
        <> map renderFixityLine fx.fixities
        <> renderMarginalia fx.marginalia
        <> note
    )

renderFixityLine :: forall w i. { alias :: String, code :: String } -> HH.HTML w i
renderFixityLine f =
  HH.div [ cls "fixity-line" ]
    [ cell "name" (glyphed f.alias)
    , cell "sep" []
    , cell "type" (decorate Html.fixity f.code)
    ]

-- ----------------------------------------------------------------------------
-- Data / newtype — the lever-frame look: each constructor on its own
-- nested subgrid row, with `=`/`|` landing in the shared [colon] track
-- so the sum's spine aligns with every `::` in the document. Record
-- payloads keep their hand-formatted brace block.

renderData :: forall w i. Array (HH.HTML w i) -> DataBlock -> HH.HTML w i
renderData note d =
  row (if d.isNewtype then [ "kind-data", "kind-newtype" ] else [ "kind-data" ])
    ( [ label (if d.isNewtype then "Newtype" else "Data") ]
        <> renderKindSig d.kindSig
        <> [ cell "name"
               [ HH.span [ cls "kw" ] [ HH.text (if d.isNewtype then "newtype " else "data ") ]
               , HH.text d.name
               ]
           ]
        <> Array.mapWithIndex renderCtorLine d.ctors
        <> (if Array.null d.payload then [] else [ preBody d.payload ])
        <> renderMarginalia d.marginalia
        <> note
    )

renderCtorLine :: forall w i. Int -> { name :: String, args :: String, comment :: Maybe String } -> HH.HTML w i
renderCtorLine i ctor =
  HH.div [ cls "ctor-line" ]
    [ HH.span [ cls "ctor-sep" ] [ HH.text (if i == 0 then "=" else "|") ]
    , HH.span [ cls "ctor-body" ]
        ( [ HH.span [ cls "ctor-name" ] [ HH.text ctor.name ] ]
            <> (if ctor.args == "" then []
                else [ HH.text " ", HH.span [ cls "ctor-args" ] (glyphed ctor.args) ])
            <> (case ctor.comment of
                  Just c -> [ HH.span [ cls "ctor-comment" ] [ HH.text c ] ]
                  Nothing -> [])
        )
    ]

-- ----------------------------------------------------------------------------
-- Type alias — name and `=` in the shared tracks; a multi-line
-- right-hand side (record aliases) keeps its source geometry in a
-- defn-body block.

renderTypeAlias :: forall w i. Array (HH.HTML w i) -> TypeAliasBlock -> HH.HTML w i
renderTypeAlias note t =
  row [ "kind-type" ]
    ( [ label "Type" ]
        <> renderKindSig t.kindSig
        <> [ cell "name" [ HH.span [ cls "kw" ] [ HH.text "type " ], HH.text t.name ] ]
        <> (case t.rhs of
              [] -> []
              [ single ] -> [ cell "sep" [ HH.text "=" ], cell "type" (glyphed single) ]
              lines -> [ cell "sep" [ HH.text "=" ], cell "type" [], preBody lines ])
        <> renderMarginalia t.marginalia
        <> note
    )

-- | A standalone kind-signature line preceding a data/newtype/type
-- | declaration, typeset through the same name/::/type tracks as every
-- | other signature in the document, with its keyword quieted.
renderKindSig :: forall w i. Maybe String -> Array (HH.HTML w i)
renderKindSig = case _ of
  Nothing -> []
  Just sig -> case String.indexOf (Pattern " :: ") sig of
    Just i ->
      [ HH.div [ cls "decl-kind-sig" ]
          [ cell "name" (decorate quietKeywords (String.take i sig))
          , cell "sep" [ HH.text "::" ]
          , cell "type" (glyphed (String.drop (i + 4) sig))
          ]
      ]
    Nothing ->
      [ HH.div [ cls "decl-kind-sig" ] [ cell "name" (decorate quietKeywords sig) ] ]

-- ----------------------------------------------------------------------------
-- Role declarations — `type role T nominal representational …`, the
-- compiler directive fixing a type's parameter roles. The type takes
-- the name column (keyword quieted), the role list sits in the type
-- column in the margin voice.

renderRoles :: forall w i. Array RoleLine -> HH.HTML w i
renderRoles rs = row [ "kind-role" ] ([ label "Roles" ] <> map renderRoleLine rs)

renderRoleLine :: forall w i. RoleLine -> HH.HTML w i
renderRoleLine r =
  HH.div [ cls "fixity-line" ]
    [ cell "name" [ HH.span [ cls "kw" ] [ HH.text "type role " ], HH.text r.name ]
    , cell "sep" []
    , HH.span [ clsx [ "type", "role-list" ] ] [ HH.text r.roles ]
    ]

-- ----------------------------------------------------------------------------
-- Section headings — an all-comment chunk in the body is the author's
-- own section divider. The decoration lines (runs of ━ ─ - = etc.) are
-- a rule drawn in the only medium a comment allows; we typeset the real
-- rule and set the text as a title.

renderSection :: forall w i. Array String -> HH.HTML w i
renderSection chunk =
  row [ "kind-section" ]
    [ HH.div [ cls "section-title" ]
        (Array.intersperse HH.br_ (map HH.text titles))
    ]
  where
  titles =
    Array.filter (\s -> s /= "" && not (isRule s))
      (map (String.trim <<< stripComment) chunk)

  stripComment l =
    let t = String.trim l in
    fromMaybe t (Array.head (Array.catMaybes
      [ String.stripPrefix (Pattern "-- |") t
      , String.stripPrefix (Pattern "--") t
      ]))

-- | A comment line made only of rule-drawing characters carries no
-- | text — it is the author approximating a hairline.
isRule :: String -> Boolean
isRule = SCU.toCharArray >>> Array.all isRuleChar
  where
  isRuleChar c =
    c == ' ' || c == '\t' || c == '-' || c == '=' || c == '_'
      || c == '*' || c == '~' || c == '#'
      || c == '━' || c == '─' || c == '═' || c == '▔'

-- ----------------------------------------------------------------------------
-- Imports

renderImports :: forall w i. Array ImportLine -> HH.HTML w i
renderImports lines =
  row [ "kind-imports" ]
    [ label "Imports"
    , HH.div [ cls "specimen-imports" ] (Array.concatMap renderImportLine lines)
    ]

renderImportLine :: forall w i. ImportLine -> Array (HH.HTML w i)
renderImportLine { qualified, mod, alias, items } =
  [ HH.span [ cls "kw" ] [ HH.text (if qualified then "import qualified" else "import") ]
  , HH.span [ cls "mod" ] [ HH.text mod ]
  , HH.span [ cls "as" ] [ HH.text (maybe' "" (\a -> "as " <> a) alias) ]
  , HH.span [ cls "list" ] [ HH.text (maybe' "" (\s -> "(" <> s <> ")") items) ]
  ]

maybe' :: forall a. String -> (a -> String) -> Maybe a -> String
maybe' empty f = case _ of
  Just a -> f a
  Nothing -> empty

-- ----------------------------------------------------------------------------
-- Class

renderClass :: forall w i. Array (HH.HTML w i) -> ClassBlock -> HH.HTML w i
renderClass note { head, body, marginalia } =
  row [ "kind-class" ]
    ( [ label "Class"
      , HH.code [ cls "class-head" ] (decorate quietKeywords head)
      ]
        <> map renderClassLine (map classifyClassLine body)
        <> renderMarginalia marginalia
        <> note
    )

-- | A class-body line either parses as a method signature (`name ::
-- | type`) — in which case it becomes its own nested subgrid row so the
-- | `::` participates in the outer `.specimen-stack` column tracks — or
-- | it doesn't (default implementations etc.) and falls back to a pre
-- | block.
data ClassLine = ClassMethodSig String String | ClassRaw String

classifyClassLine :: String -> ClassLine
classifyClassLine raw =
  let trimmed = String.trim raw in
  case String.indexOf (Pattern " :: ") trimmed of
    Just i -> ClassMethodSig (String.take i trimmed) (String.drop (i + 4) trimmed)
    Nothing -> ClassRaw raw

renderClassLine :: forall w i. ClassLine -> HH.HTML w i
renderClassLine = case _ of
  ClassMethodSig name sig ->
    HH.div [ cls "class-method" ]
      [ cell "name" [ HH.text name ]
      , cell "sep" [ HH.text "::" ]
      , cell "type" (renderType sig)
      ]
  ClassRaw raw -> preBody [ raw ]

-- ----------------------------------------------------------------------------
-- Instance — split `instance NAME :: TYPE` into name/sep/type so the
-- `::` aligns with value-decl sigs through the shared subgrid.

renderInstance :: forall w i. Array (HH.HTML w i) -> InstanceBlock -> HH.HTML w i
renderInstance note { head, body, marginalia } =
  let gathered = gatherInstanceHead head body in
  row [ "kind-instance" ]
    ( [ label "Instance" ]
        <> renderInstanceSig (parseInstanceHead gathered.headFull)
        <> renderBody gathered.rest
        <> renderMarginalia marginalia
        <> note
    )

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
  Just _ -> true
  Nothing -> false

-- | Typeset an instance head. Named instances split at ` :: ` into the
-- | name column and a signature; anonymous instances put everything
-- | after the (quieted) keyword run through the signature machinery.
-- | The trailing `where` is set at the sig's foot in the head weight —
-- | the same emphasis it gets in a class head.
renderInstanceSig :: forall w i. { name :: String, sig :: Maybe String } -> Array (HH.HTML w i)
renderInstanceSig parts = case parts.sig of
  Just s ->
    [ cell "name" (decorate quietKeywords parts.name)
    , cell "sep" [ HH.text "::" ]
    , cell "type" (instanceType s)
    ]
  Nothing -> case splitLeadingInstanceKeywords parts.name of
    Just { kws, rest } ->
      [ cell "name" [ HH.span [ cls "kw" ] [ HH.text kws ] ]
      , cell "sep" []
      , cell "type" (instanceType rest)
      ]
    Nothing ->
      [ cell "name" (decorate quietKeywords parts.name)
      , cell "sep" []
      , cell "type" []
      ]

instanceType :: forall w i. String -> Array (HH.HTML w i)
instanceType s =
  renderType stripped.sig
    <> (if stripped.hasWhere
        then [ HH.span [ cls "inst-where" ] [ HH.text " where" ] ]
        else [])
  where
  stripped = case String.stripSuffix (Pattern " where") (String.trim s) of
    Just s' -> { sig: s', hasWhere: true }
    Nothing -> { sig: String.trim s, hasWhere: false }

-- | Split the leading keyword run off an anonymous instance head.
-- | Longest candidates first.
splitLeadingInstanceKeywords :: String -> Maybe { kws :: String, rest :: String }
splitLeadingInstanceKeywords s0 =
  Array.head (Array.mapMaybe try instanceKeywords)
  where
  s = String.trim s0
  try p = case String.stripPrefix (Pattern (p <> " ")) s of
    Just rest | rest /= "" -> Just { kws: p, rest }
    _ -> Nothing

parseInstanceHead :: String -> { name :: String, sig :: Maybe String }
parseInstanceHead h =
  let trimmed = String.trim h in
  case String.indexOf (Pattern " :: ") trimmed of
    Just i -> { name: String.take i trimmed, sig: Just (String.drop (i + 4) trimmed) }
    Nothing -> { name: trimmed, sig: Nothing }

-- ----------------------------------------------------------------------------
-- Value

renderValue :: forall w i. Array (HH.HTML w i) -> ValueBlock -> HH.HTML w i
renderValue note v =
  row [ "kind-value" ]
    ( renderSig v.name v.sig
        <> renderBody v.body
        <> renderMarginalia v.marginalia
        <> note
        # Array.cons (label "Function")
    )

-- | Emit name + :: + type as direct grid items so the outer
-- | `.specimen-stack` aligns them vertically across siblings.
renderSig :: forall w i. String -> Maybe String -> Array (HH.HTML w i)
renderSig name = case _ of
  Nothing ->
    [ HH.span [ clsx [ "name", "nameonly" ] ] [ HH.text name ]
    , cell "sep" []
    , cell "type" []
    ]
  Just sig ->
    [ cell "name" [ HH.text name ]
    , cell "sep" [ HH.text "::" ]
    , cell "type" (renderType sig)
    ]

-- | A type expression: inline when it fits the column, otherwise
-- | broken into a vertical stack of stations. Both are colorized
-- | against the ∀-bound type variables when present.
renderType :: forall w i. String -> Array (HH.HTML w i)
renderType sig =
  if shouldStack sig then [ renderStacked colors (segmentSig sig) ]
  else Html.decorated colors sig
  where
  colors = assignColors (forallVars sig)

-- | Vertical layout: each station emits a body span (left, type-text)
-- | and an op span (right column, holds the trailing →/⇒/.). The two
-- | spans participate as direct items of `.sig-stack`'s auto/auto grid
-- | so the connectors hug the right edge of the longest body rather
-- | than floating at the cell's far right.
renderStacked :: forall w i. Map String String -> Array Station -> HH.HTML w i
renderStacked colors stations0 =
  HH.div [ cls "sig-stack" ]
    (Array.concat (Array.mapWithIndex (\i s -> renderStation colors (Just i == firstArgIdx) s) stations))
  where
  stations = Array.concatMap splitTupleConstraint stations0

  -- The boundary is the first non-constraint station after at least
  -- one constraint — i.e. the start of the function's actual shape.
  -- Marked so we can give it a small breath of vertical space.
  firstArgIdx = map (_ + 1) (Array.findLastIndex isConstraint stations)

  isConstraint s = case s.connector of
    ConConstraint -> true
    _ -> false

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
renderStation :: forall w i. Map String String -> Boolean -> Station -> Array (HH.HTML w i)
renderStation colors argsStart { body, connector } =
  [ HH.span [ clsx (["sig-station-body"] <> boundary <> prose) ] bodyHtml
  , HH.span [ clsx (["sig-station-op"] <> opClass <> boundary) ] opHtml
  ]
  where
  bodyHtml = Html.decorated colors body <> case connector of
    ConDot -> [ HH.small [ cls "sig-forall-dot" ] [ HH.text " ." ] ]
    _ -> []

  opHtml = case connector of
    ConConstraint -> [ HH.span [ cls "g" ] [ HH.text "⇒" ] ]
    ConArrow -> [ HH.span [ cls "g" ] [ HH.text "→" ] ]
    _ -> []

  opClass = case connector of
    ConConstraint -> [ "sig-op-constraint" ]
    ConArrow -> [ "sig-op-arrow" ]
    _ -> []

  boundary = if argsStart then [ "sig-args-start" ] else []

  -- A station carrying a string literal (Warn/Text messages, Fail
  -- constraints) is prose, and prose wraps; type geometry never does.
  prose = if String.contains (Pattern "\"") body then [ "sig-station-prose" ] else []

-- | Single-line bodies of the form `name args = expr` render as a
-- | nested subgrid so the leading identifier aligns with the sig's
-- | name above and the `=` lands in the same column as the sig's `::`.
-- | Multi-line bodies (do-blocks, where-clauses, case expressions) keep
-- | the pre fallback because their geometry doesn't fit the 3-col shape.
renderBody :: forall w i. Array String -> Array (HH.HTML w i)
renderBody = case _ of
  [] -> []
  [ single ] -> case parseSingleLineBody single of
    Just { lhs, rhs } ->
      [ HH.div [ cls "value-body" ]
          [ cell "name" [ HH.text lhs ]
          , cell "sep" [ HH.text "=" ]
          , cell "type" (glyphed rhs)
          ]
      ]
    Nothing -> [ preBody [ single ] ]
  lines -> [ preBody lines ]

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

-- ----------------------------------------------------------------------------
-- Raw fallback

renderRaw :: forall w i. Array String -> HH.HTML w i
renderRaw lines =
  row [ "kind-raw" ]
    [ HH.span [ cls "label" ] []
    , HH.pre [ cls "raw" ] [ HH.text (String.joinWith "\n" lines) ]
    ]

-- ----------------------------------------------------------------------------
-- Label (left gutter) + Marginalia (full code-area width, below body)

renderMarginalia :: forall w i. Array String -> Array (HH.HTML w i)
renderMarginalia notes
  | Array.null notes = []
  | otherwise =
      [ HH.div
          [ clsx (if isCompactMarginalia notes
                  then [ "marginalia", "marginalia-compact" ]
                  else [ "marginalia" ]) ]
          (docLines notes)
      ]

-- ----------------------------------------------------------------------------
-- Foot

renderFoot :: forall w i. String -> HH.HTML w i
renderFoot source =
  HH.footer [ cls "specimen-foot" ]
    [ HH.div [ cls "field" ] [ HH.text ("module from " <> source) ] ]

-- ----------------------------------------------------------------------------
-- Shared vocabulary
--
-- The document is one big CSS subgrid; these four helpers name the
-- pieces of that contract so the renderers below read as layout rather
-- than as markup.

-- | A top-level declaration row in the stack.
row :: forall w i. Array String -> Array (HH.HTML w i) -> HH.HTML w i
row kinds = HH.section [ clsx ([ "row" ] <> kinds) ]

-- | One of the shared column tracks: `name`, `sep`, `type`, `label`.
cell :: forall w i. String -> Array (HH.HTML w i) -> HH.HTML w i
cell track = HH.span [ cls track ]

-- | The left-gutter kind label.
label :: forall w i. String -> HH.HTML w i
label = cell "label" <<< Array.singleton <<< HH.text

-- | Source that keeps its own geometry.
preBody :: forall w i. Array String -> HH.HTML w i
preBody lines = HH.pre [ cls "defn-body" ] (glyphed (String.joinWith "\n" lines))

-- ----------------------------------------------------------------------------
-- Decoration

-- | Run a `Specimen.Html` pass, then glyph emphasis, then emit.
decorate :: forall w i. (String -> Array Token) -> String -> Array (HH.HTML w i)
decorate pass = pass >>> Html.onPlain Html.glyphs >>> Html.toHtml

glyphed :: forall w i. String -> Array (HH.HTML w i)
glyphed = Html.glyphed

-- | Quiet the leading declaration keyword(s): the left-gutter label
-- | already names the block kind, so `class` / `instance` in the code
-- | line is typeset in the margin colour — the same emphasis treatment
-- | the imports column gives the `import` keyword.
quietKeywords :: String -> Array Token
quietKeywords = Html.keyword declKeywords

declKeywords :: Array String
declKeywords = instanceKeywords <> [ "class", "newtype", "data", "type" ]

-- | Instance-head keyword runs, longest first — `stripPrefix` takes the
-- | first match, so `newtype instance` must be offered before `newtype`.
instanceKeywords :: Array String
instanceKeywords =
  [ "else derive newtype instance"
  , "else derive instance"
  , "else newtype instance"
  , "else instance"
  , "derive newtype instance"
  , "derive instance"
  , "newtype instance"
  , "instance"
  ]

-- ----------------------------------------------------------------------------
-- Halogen conveniences

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< ClassName

clsx :: forall r i. Array String -> HP.IProp (class :: String | r) i
clsx = HP.classes <<< map ClassName
