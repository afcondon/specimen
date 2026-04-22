module Specimen.Markdown
  ( docLinesToHtml
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

-- | A parsed marginalia node — either prose or a "law list" (consecutive
-- | `- name: code` items rendered as a 3-column aligned-colon grid).
data MdNode
  = Para  String
  | LawList (Array { name :: String, code :: String })

-- | Render an array of stripped doc-comment lines as HTML.
-- | Pipeline:
-- |   1. drop fence markers (content flows into surrounding paragraphs as text)
-- |   2. group lines into paragraphs (blank line and `- ` both break)
-- |   3. classify each paragraph as Para or LawList
-- |   4. consolidate adjacent LawLists into one block (so colons align)
-- |   5. render
docLinesToHtml :: Array String -> String
docLinesToHtml lines =
  let
    paras = groupParagraphs (Array.filter (not <<< isFenceMarker) lines)
    nodes = consolidateNodes (map paraToNode paras)
  in
    String.joinWith "" (map renderNode nodes)

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

renderNode :: MdNode -> String
renderNode = case _ of
  Para s         -> renderParagraph s
  LawList items  -> renderLawList items

renderParagraph :: String -> String
renderParagraph s =
  let
    trimmed = String.trim s
    body    = escape trimmed # stripPursuitLinks # renderInlineCode
    -- Only paragraphs whose first character is a plain letter get the
    -- drop cap. Paragraphs starting with `code`, [link], etc. don't —
    -- ::first-letter would otherwise target a letter inside markup.
    cls = if startsWithLetter trimmed then " class=\"can-dropcap\"" else ""
  in
    if body == "" then "" else "<p" <> cls <> ">" <> body <> "</p>"

startsWithLetter :: String -> Boolean
startsWithLetter s = case regex "^[A-Za-z]" noFlags of
  Right r -> test r s
  Left _  -> false

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
