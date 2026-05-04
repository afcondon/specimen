module Specimen.Markdown
  ( docLinesToHtml
  , isCompactMarginalia
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (regex, replace, test)
import Data.String.Regex.Flags (global, noFlags)

-- | A parsed marginalia node — either prose, a "law list" (consecutive
-- | `- name: code` items rendered as a 3-column aligned-colon grid), or
-- | a fenced code block (markdown ``` … ``` lifted out as a quoted code
-- | excerpt).
data MdNode
  = Para  String
  | LawList (Array { name :: String, code :: String })
  | Code (Array String)

-- | Render an array of stripped doc-comment lines as HTML.
-- | Pipeline:
-- |   1. split on fence markers — prose runs go through the paragraph
-- |      pipeline; fenced runs become Code nodes verbatim
-- |   2. group prose lines into paragraphs (blank line and `- ` both break)
-- |   3. classify each paragraph as Para or LawList
-- |   4. consolidate adjacent LawLists into one block (so colons align)
-- |   5. render — the first node gets headline treatment (drop cap for
-- |      prose-led; pulled-out code subhead for code-led), the rest stay
-- |      as plain paragraphs.
docLinesToHtml :: Array String -> String
docLinesToHtml lines =
  let
    chunks = splitOnFences lines
    nodes = consolidateNodes (Array.concatMap chunkToNodes chunks)
  in
    case Array.uncons nodes of
      Nothing -> ""
      Just { head, tail } ->
        renderNode true head
          <> String.joinWith "" (map (renderNode false) tail)

-- | A chunk is either a run of prose lines or a run of fenced-code lines.
-- | We tag with a Boolean: true = code, false = prose.
splitOnFences :: Array String -> Array { isCode :: Boolean, lines :: Array String }
splitOnFences lines = go [] [] false lines
  where
  flush acc cur isCode =
    if Array.null cur then acc
    else Array.snoc acc { isCode, lines: cur }

  go acc cur isCode xs = case Array.uncons xs of
    Nothing -> flush acc cur isCode
    Just { head, tail }
      | isFenceMarker head -> go (flush acc cur isCode) [] (not isCode) tail
      | otherwise          -> go acc (Array.snoc cur head) isCode tail

chunkToNodes :: { isCode :: Boolean, lines :: Array String } -> Array MdNode
chunkToNodes { isCode: true,  lines } = [ Code lines ]
chunkToNodes { isCode: false, lines } = map paraToNode (groupParagraphs lines)

isFenceMarker :: String -> Boolean
isFenceMarker line =
  case String.stripPrefix (Pattern "```") (String.trim line) of
    Just _  -> true
    Nothing -> false

-- | Group lines into paragraphs. Blank lines AND lines starting with
-- | `- ` both break the current paragraph — a list item is its own
-- | one-line paragraph so the law-grouping pass can pick it up.
groupParagraphs :: Array String -> Array (Array String)
groupParagraphs lines = go [] [] lines
  where
  go acc cur xs = case Array.uncons xs of
    Nothing -> if Array.null cur then acc else Array.snoc acc cur
    Just { head, tail }
      | String.trim head == "" ->
          if Array.null cur
            then go acc [] tail
            else go (Array.snoc acc cur) [] tail
      | startsWithListMarker head ->
          let acc' = if Array.null cur then acc else Array.snoc acc cur
          in go acc' [head] tail
      | otherwise ->
          go acc (Array.snoc cur head) tail

startsWithListMarker :: String -> Boolean
startsWithListMarker s = case String.stripPrefix (Pattern "- ") (String.trim s) of
  Just _  -> true
  Nothing -> false

-- | A marginalia block is "compact" when it parses to a single short
-- | prose paragraph that would also receive a drop cap — i.e. starts
-- | with a plain letter. The cream-block + drop-cap pull-quote treatment
-- | reads as over-produced for one-sentence comments; the renderer
-- | switches to a flatter, in-flow rendering when this returns true.
-- | Threshold of 100 chars ≈ one line at 1.05rem inside the 58rem block.
isCompactMarginalia :: Array String -> Boolean
isCompactMarginalia lines =
  let
    chunks = splitOnFences lines
    nodes  = consolidateNodes (Array.concatMap chunkToNodes chunks)
  in case nodes of
    [ Para s ] ->
      let trimmed = String.trim s
      in String.length trimmed > 0
         && String.length trimmed < 100
         && startsWithLetter trimmed
    _ -> false

-- ----------------------------------------------------------------------------
-- Paragraph → node classification

paraToNode :: Array String -> MdNode
paraToNode ls =
  let joined = String.joinWith " " ls
  in case parseLawItem joined of
    Just law -> LawList [law]
    Nothing  -> Para joined

-- | A "law" item is any list item with a `:` separator, like
-- | `- Left Identity: pure x >>= f = f x`.
parseLawItem :: String -> Maybe { name :: String, code :: String }
parseLawItem raw = do
  rest <- String.stripPrefix (Pattern "- ") (String.trim raw)
  i <- String.indexOf (Pattern ": ") rest
  pure
    { name: String.trim (String.take i rest)
    , code: String.trim (String.drop (i + 2) rest)
    }

-- | Merge adjacent LawLists into one so they share a single grid (and
-- | therefore a single set of column tracks — the alignment we want).
consolidateNodes :: Array MdNode -> Array MdNode
consolidateNodes = Array.foldl step []
  where
  step acc node = case Array.unsnoc acc of
    Just { init, last: LawList items } -> case node of
      LawList more -> Array.snoc init (LawList (items <> more))
      _            -> Array.snoc acc node
    _ -> Array.snoc acc node

-- ----------------------------------------------------------------------------
-- Rendering

renderNode :: Boolean -> MdNode -> String
renderNode isFirst = case _ of
  Para s         -> renderParagraph isFirst s
  LawList items  -> renderLawList items
  Code lines     -> renderCodeBlock lines

-- | Render a fenced code block as a quoted excerpt — pre-formatted,
-- | monospace, anchored by a left rule (see .marginalia pre.code-quote
-- | in style.css). Empty blocks are dropped.
renderCodeBlock :: Array String -> String
renderCodeBlock lines =
  let nonEmpty = Array.dropWhile isAllBlank (Array.reverse (Array.dropWhile isAllBlank (Array.reverse lines)))
  in if Array.null nonEmpty then ""
     else
       "<pre class=\"code-quote\"><code>"
         <> String.joinWith "\n" (map escape nonEmpty)
         <> "</code></pre>"
  where
  isAllBlank l = String.trim l == ""

renderParagraph :: Boolean -> String -> String
renderParagraph isFirst s =
  let
    trimmed = String.trim s
    body    = escape trimmed # stripPursuitLinks # renderInlineCode
  in
    if body == "" then ""
    else if isFirst && startsWithBacktick trimmed then
      renderCodeLed body
    else if isFirst && startsWithLetter trimmed then
      "<p class=\"can-dropcap\">" <> body <> "</p>"
    else
      "<p>" <> body <> "</p>"

-- | Pull the leading <code>X</code> out as a block-level subhead so the
-- | comment block has a strong typographic opening even when the doc
-- | starts with the function's own identifier (e.g. ``liftM1``).
renderCodeLed :: String -> String
renderCodeLed body = case extractLeadingCode body of
  Just { code, rest } ->
    "<p class=\"code-led\">"
      <> "<span class=\"code-cap\">" <> code <> "</span>"
      <> String.trim rest
      <> "</p>"
  Nothing ->
    "<p>" <> body <> "</p>"

extractLeadingCode :: String -> Maybe { code :: String, rest :: String }
extractLeadingCode body = do
  rest1 <- String.stripPrefix (Pattern "<code>") body
  i <- String.indexOf (Pattern "</code>") rest1
  pure
    { code: String.take i rest1
    , rest: String.drop (i + 7) rest1   -- 7 = length of "</code>"
    }

startsWithLetter :: String -> Boolean
startsWithLetter s = case regex "^[A-Za-z]" noFlags of
  Right r -> test r s
  Left _  -> false

startsWithBacktick :: String -> Boolean
startsWithBacktick s = case String.stripPrefix (Pattern "`") s of
  Just _  -> true
  Nothing -> false

renderLawList :: Array { name :: String, code :: String } -> String
renderLawList items =
  "<div class=\"laws\">"
    <> String.joinWith "" (map renderLaw items)
    <> "</div>"

renderLaw :: { name :: String, code :: String } -> String
renderLaw { name, code } =
  "<div class=\"law-name\">" <> escape name <> "</div>"
    <> "<div class=\"law-sep\">:</div>"
    <> "<div class=\"law-code\"><code>" <> escape code <> "</code></div>"

-- ----------------------------------------------------------------------------
-- Inline markdown

-- | `[text](url-or-anchor)` → `<strong>text</strong>`.
stripPursuitLinks :: String -> String
stripPursuitLinks s = case regex "\\[([^\\]]+)\\]\\([^)]*\\)" global of
  Right r -> replace r "<strong>$1</strong>" s
  Left _  -> s

-- | `` `code` `` → `<code>code</code>`.
renderInlineCode :: String -> String
renderInlineCode s = case regex "`([^`]+)`" global of
  Right r -> replace r "<code>$1</code>" s
  Left _  -> s

-- ----------------------------------------------------------------------------
-- HTML escape

escape :: String -> String
escape = replaceAll (Pattern "&")  (Replacement "&amp;")
     >>> replaceAll (Pattern "<")  (Replacement "&lt;")
     >>> replaceAll (Pattern ">")  (Replacement "&gt;")
     >>> replaceAll (Pattern "\"") (Replacement "&quot;")
