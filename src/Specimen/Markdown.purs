-- | The little markdown Specimen understands, as found in PureScript
-- | doc comments: paragraphs, fenced code, `- Name: law` lists, inline
-- | code and Pursuit links.
-- |
-- | Both the block grammar and the inline grammar are parsed to values
-- | (`MdNode`, `Inline`) and rendered from those. The inline parser in
-- | particular used to be two regex substitutions over escaped HTML,
-- | which meant `renderCodeLed` had to go looking for a literal
-- | `"<code>"` in its own output to decide the opening treatment; it
-- | now just asks what the first inline node is.
module Specimen.Markdown
  ( docLines
  , isCompactMarginalia
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))

import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP

-- | A parsed marginalia node — either prose, a "law list" (consecutive
-- | `- name: code` items rendered as a 3-column aligned-colon grid), or
-- | a fenced code block (markdown ``` … ``` lifted out as a quoted code
-- | excerpt).
data MdNode
  = Para String
  | LawList (Array Law)
  | Code (Array String)

type Law = { name :: String, code :: String }

-- | A span of prose: plain text, `inline code`, or the text of a
-- | `[link](url)` (Specimen keeps the words, drops the destination —
-- | a printed specimen has nowhere to click to). Link text is itself
-- | prose: Pursuit links are routinely written ``[`Monad`](…)``, and
-- | the backticks inside have to become code, not literal backticks.
data Inline
  = InText String
  | InCode String
  | InStrong (Array Inline)

-- | Render an array of stripped doc-comment lines.
-- |
-- | Pipeline:
-- |   1. split on fence markers — prose runs go through the paragraph
-- |      pipeline; fenced runs become Code nodes verbatim
-- |   2. group prose lines into paragraphs (blank line and `- ` both break)
-- |   3. classify each paragraph as Para or LawList
-- |   4. consolidate adjacent LawLists into one block (so colons align)
-- |   5. render — the first node gets headline treatment (drop cap for
-- |      prose-led; pulled-out code subhead for code-led), the rest stay
-- |      as plain paragraphs.
docLines :: forall w i. Array String -> Array (HH.HTML w i)
docLines lines =
  case Array.uncons (parseNodes lines) of
    Nothing -> []
    Just { head, tail } ->
      renderNode true head <> Array.concatMap (renderNode false) tail

parseNodes :: Array String -> Array MdNode
parseNodes =
  splitOnFences >>> Array.concatMap chunkToNodes >>> consolidateNodes

-- | A chunk is either a run of prose lines or a run of fenced-code lines.
splitOnFences :: Array String -> Array { isCode :: Boolean, lines :: Array String }
splitOnFences = go [] [] false
  where
  flush acc cur isCode =
    if Array.null cur then acc
    else Array.snoc acc { isCode, lines: cur }

  go acc cur isCode xs = case Array.uncons xs of
    Nothing -> flush acc cur isCode
    Just { head, tail }
      | isFenceMarker head -> go (flush acc cur isCode) [] (not isCode) tail
      | otherwise -> go acc (Array.snoc cur head) isCode tail

chunkToNodes :: { isCode :: Boolean, lines :: Array String } -> Array MdNode
chunkToNodes { isCode, lines } =
  if isCode then [ Code lines ]
  else map paraToNode (groupParagraphs lines)

isFenceMarker :: String -> Boolean
isFenceMarker line = hasPrefix "```" (String.trim line)

-- | Group lines into paragraphs. Blank lines AND lines starting with
-- | `- ` both break the current paragraph — a list item is its own
-- | one-line paragraph so the law-grouping pass can pick it up.
groupParagraphs :: Array String -> Array (Array String)
groupParagraphs = go [] []
  where
  go acc cur xs = case Array.uncons xs of
    Nothing -> if Array.null cur then acc else Array.snoc acc cur
    Just { head, tail }
      | String.trim head == "" ->
          if Array.null cur then go acc [] tail
          else go (Array.snoc acc cur) [] tail
      | startsWithListMarker head ->
          let acc' = if Array.null cur then acc else Array.snoc acc cur
          in go acc' [ head ] tail
      | otherwise -> go acc (Array.snoc cur head) tail

startsWithListMarker :: String -> Boolean
startsWithListMarker s = hasPrefix "- " (String.trim s)

-- | A marginalia block is "compact" when it parses to a single short
-- | prose paragraph that would also receive a drop cap — i.e. starts
-- | with a plain letter. The cream-block + drop-cap pull-quote treatment
-- | reads as over-produced for one-sentence comments; the renderer
-- | switches to a flatter, in-flow rendering when this returns true.
-- | Threshold of 100 chars ≈ one line at 1.05rem inside the 58rem block.
isCompactMarginalia :: Array String -> Boolean
isCompactMarginalia lines = case parseNodes lines of
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
    Just law -> LawList [ law ]
    Nothing -> Para joined

-- | A "law" item is any list item with a `:` separator, like
-- | `- Left Identity: pure x >>= f = f x`.
parseLawItem :: String -> Maybe Law
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
  step acc node = case Array.unsnoc acc, node of
    Just { init, last: LawList items }, LawList more ->
      Array.snoc init (LawList (items <> more))
    _, _ -> Array.snoc acc node

-- ----------------------------------------------------------------------------
-- Inline grammar

-- | Scan prose for the two inline forms Specimen recognises. Anything
-- | unterminated (a lone backtick, a `[` with no closing `](…)`) stays
-- | literal text, which is what a doc comment discussing syntax wants.
parseInline :: String -> Array Inline
parseInline = go [] []
  where
  go acc cur s = case SCU.uncons s of
    Nothing -> flush acc cur
    Just { head: '`', tail }
      | Just { inner, rest } <- delimited "`" tail ->
          go (Array.snoc (flush acc cur) (InCode inner)) [] rest
    Just { head: '[', tail }
      | Just { inner, rest } <- linkText tail ->
          go (Array.snoc (flush acc cur) (InStrong (parseInline inner))) [] rest
    Just { head: c, tail } -> go acc (Array.snoc cur c) tail

  flush acc cur =
    if Array.null cur then acc
    else Array.snoc acc (InText (SCU.fromCharArray cur))

-- | Content up to the next `close`, plus what follows it.
delimited :: String -> String -> Maybe { inner :: String, rest :: String }
delimited close s = do
  i <- String.indexOf (Pattern close) s
  pure
    { inner: String.take i s
    , rest: String.drop (i + String.length close) s
    }

-- | `text](url)` → the text, with the destination discarded.
linkText :: String -> Maybe { inner :: String, rest :: String }
linkText s = do
  { inner, rest } <- delimited "](" s
  { rest: after } <- delimited ")" rest
  pure { inner, rest: after }

renderInline :: forall w i. Array Inline -> Array (HH.HTML w i)
renderInline = map case _ of
  InText s -> HH.text s
  InCode s -> HH.code_ [ HH.text s ]
  InStrong inner -> HH.strong_ (renderInline inner)

-- ----------------------------------------------------------------------------
-- Rendering

renderNode :: forall w i. Boolean -> MdNode -> Array (HH.HTML w i)
renderNode isFirst = case _ of
  Para s -> renderParagraph isFirst s
  LawList items -> [ HH.div [ cls "laws" ] (Array.concatMap renderLaw items) ]
  Code lines -> renderCodeBlock lines

-- | Render a fenced code block as a quoted excerpt — pre-formatted,
-- | monospace, anchored by a left rule (see .marginalia pre.code-quote
-- | in style.css). Empty blocks are dropped.
renderCodeBlock :: forall w i. Array String -> Array (HH.HTML w i)
renderCodeBlock lines =
  case trimBlank lines of
    [] -> []
    kept ->
      [ HH.pre [ cls "code-quote" ]
          [ HH.code_ [ HH.text (String.joinWith "\n" kept) ] ]
      ]

trimBlank :: Array String -> Array String
trimBlank =
  Array.dropWhile isBlank
    >>> Array.reverse
    >>> Array.dropWhile isBlank
    >>> Array.reverse
  where
  isBlank l = String.trim l == ""

renderParagraph :: forall w i. Boolean -> String -> Array (HH.HTML w i)
renderParagraph isFirst s =
  let trimmed = String.trim s in
  case parseInline trimmed of
    [] -> []
    -- A doc comment opening on the function's own identifier gets that
    -- identifier pulled out as a block-level subhead, so the block still
    -- has a strong typographic opening.
    inlines | isFirst, Just { head: InCode code, tail } <- Array.uncons inlines ->
      [ codeLed code (renderInline tail) ]
    inlines
      | isFirst && startsWithLetter trimmed ->
          [ HH.p [ cls "can-dropcap" ] (renderInline inlines) ]
      | otherwise -> [ HH.p_ (renderInline inlines) ]

codeLed :: forall w i. String -> Array (HH.HTML w i) -> HH.HTML w i
codeLed code rest =
  HH.p [ cls "code-led" ] ([ HH.span [ cls "code-cap" ] [ HH.text code ] ] <> rest)

renderLaw :: forall w i. Law -> Array (HH.HTML w i)
renderLaw { name, code } =
  [ HH.div [ cls "law-name" ] [ HH.text name ]
  , HH.div [ cls "law-sep" ] [ HH.text ":" ]
  , HH.div [ cls "law-code" ] [ HH.code_ [ HH.text code ] ]
  ]

-- ----------------------------------------------------------------------------
-- Helpers

startsWithLetter :: String -> Boolean
startsWithLetter s = case SCU.charAt 0 s of
  Just c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  Nothing -> false

hasPrefix :: String -> String -> Boolean
hasPrefix p s = case String.stripPrefix (Pattern p) s of
  Just _ -> true
  Nothing -> false

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< ClassName
