module Specimen.Render
  ( renderDocument
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))

import Specimen.Block (Block(..), Header, ImportLine, ClassBlock, InstanceBlock, ValueBlock)
import Specimen.Markdown (docLinesToHtml)

type DocOpts =
  { moduleSlug :: String
  , source     :: String
  , blocks     :: Array Block
  }

-- | Render the full document HTML.
renderDocument :: DocOpts -> String
renderDocument { moduleSlug, source, blocks } =
  let
    header = case Array.find isHeader blocks of
      Just (BHeader h) -> renderHeader h
      _                -> renderHeaderFallback moduleSlug
    nonHeader = Array.filter (not <<< isHeader) blocks
    body = renderStack nonHeader
    foot = renderFoot { source, moduleSlug }
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
      Nothing -> { kind: "value", label: "Function", name: trimmed }

renderHeaderFallback :: String -> String
renderHeaderFallback name =
  "<header class=\"specimen-head\">"
    <> "<h1>" <> escape name <> "</h1>"
    <> "</header>"

-- ----------------------------------------------------------------------------
-- Stack — one big subgrid that holds imports + class + instances + values.

renderStack :: Array Block -> String
renderStack blocks =
  "<div class=\"specimen-stack\">"
    <> String.joinWith "" (map renderBlock blocks)
    <> "</div>"

renderBlock :: Block -> String
renderBlock = case _ of
  BHeader _      -> ""  -- already rendered above the stack
  BImports is    -> renderImports is
  BClass    c    -> renderClass c
  BInstance i    -> renderInstance i
  BValue    v    -> renderValue v
  BRaw      ls   -> renderRaw ls

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

renderClass :: ClassBlock -> String
renderClass { head, body, marginalia } =
  "<section class=\"row kind-class\">"
    <> renderLabel "Class"
    <> "<code class=\"class-head\">" <> decorateGlyphs (escape head) <> "</code>"
    <> (if Array.null body
          then ""
          else "<pre class=\"defn-body\">" <> decorateGlyphs (escape (String.joinWith "\n" body)) <> "</pre>")
    <> renderMarginalia marginalia
    <> "</section>"

-- ---- Instance

renderInstance :: InstanceBlock -> String
renderInstance { head, marginalia } =
  "<section class=\"row kind-instance\">"
    <> renderLabel "Instance"
    <> "<code class=\"instance-head\">" <> decorateGlyphs (escape head) <> "</code>"
    <> renderMarginalia marginalia
    <> "</section>"

-- ---- Value

renderValue :: ValueBlock -> String
renderValue v =
  "<section class=\"row kind-value\">"
    <> renderLabel "Function"
    <> renderSig v.name v.sig
    <> renderBody v.body
    <> renderMarginalia v.marginalia
    <> "</section>"

-- | Emit name + :: + type as direct grid items so the outer .specimen-stack
-- | aligns them vertically across siblings.
renderSig :: String -> Maybe String -> String
renderSig name = case _ of
  Nothing ->
    "<span class=\"name nameonly\">" <> escape name <> "</span>"
      <> "<span class=\"sep\"></span>"
      <> "<span class=\"type\"></span>"
  Just sig ->
    "<span class=\"name\">" <> escape name <> "</span>"
      <> "<span class=\"sep\">::</span>"
      <> "<span class=\"type\">" <> decorateGlyphs (escape sig) <> "</span>"

renderBody :: Array String -> String
renderBody lines =
  if Array.null lines
    then ""
    else "<pre class=\"defn-body\">" <> decorateGlyphs (escape (String.joinWith "\n" lines)) <> "</pre>"

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
    else "<div class=\"marginalia\">" <> docLinesToHtml notes <> "</div>"

-- ----------------------------------------------------------------------------
-- Foot

renderFoot :: { source :: String, moduleSlug :: String } -> String
renderFoot { source, moduleSlug } =
  "<footer class=\"specimen-foot\">"
    <> "<div class=\"field\">Source — " <> escape source <> "</div>"
    <> "<div class=\"field\">" <> escape moduleSlug <> " · Specimen · Hylograph Showcases</div>"
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
