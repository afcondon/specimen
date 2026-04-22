module Specimen.Block
  ( Block(..)
  , Header
  , ImportLine
  , ClassBlock
  , InstanceBlock
  , ValueBlock
  , ForeignBlock
  , extractBlocks
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))

type Header        = { name :: String, exports :: Array String }
type ImportLine    = { qualified :: Boolean, mod :: String, alias :: Maybe String, items :: Maybe String }
type ClassBlock    = { head :: String, body :: Array String, marginalia :: Array String }
type InstanceBlock = { head :: String, marginalia :: Array String }
type ValueBlock    = { name :: String, sig :: Maybe String, body :: Array String, marginalia :: Array String }
type ForeignBlock  = { name :: String, sig :: String, isType :: Boolean, marginalia :: Array String }

data Block
  = BHeader   Header
  | BImports  (Array ImportLine)
  | BClass    ClassBlock
  | BInstance InstanceBlock
  | BForeign  ForeignBlock
  | BValue    ValueBlock
  | BRaw      (Array String)

-- | Extract typed blocks from a PureScript module source.
-- |
-- | Strategy (regex-light pass): split on blank-line boundaries,
-- | classify each chunk by its first non-comment line, then merge
-- | consecutive import-chunks. Doc-comment lines preceding a decl
-- | become marginalia for that decl.
extractBlocks :: String -> Array Block
extractBlocks src = mergeImports (map classifyChunk (chunkLines src))

-- ----------------------------------------------------------------------------
-- Chunking

chunkLines :: String -> Array (Array String)
chunkLines src =
  groupChunks (String.split (Pattern "\n") src)
    # Array.filter (not <<< Array.null)

-- | Group lines into decl-sized chunks. A blank line is normally a
-- | chunk boundary, EXCEPT when the next non-blank line is indented —
-- | which means it's a continuation of the current decl (e.g. a `where`
-- | clause helper definition separated from the previous one by a
-- | blank line). PureScript allows blank lines inside indented blocks.
groupChunks :: Array String -> Array (Array String)
groupChunks lines = go [] [] lines
  where
  go acc cur xs = case Array.uncons xs of
    Nothing -> if Array.null cur then acc else Array.snoc acc cur
    Just { head, tail }
      | isBlank head ->
          if Array.null cur then
            go acc [] tail
          else if nextNonBlankIsIndented tail then
            go acc (Array.snoc cur head) tail   -- interior blank
          else
            go (Array.snoc acc cur) [] tail     -- real boundary
      | otherwise -> go acc (Array.snoc cur head) tail

  nextNonBlankIsIndented xs = case Array.find (not <<< isBlank) xs of
    Just l  -> isIndented l
    Nothing -> false

isBlank :: String -> Boolean
isBlank line = String.trim line == ""

-- ----------------------------------------------------------------------------
-- Classification

classifyChunk :: Array String -> Block
classifyChunk chunk =
  let
    Tuple comments rest = peelComments chunk
    marginalia = stripDocPrefix comments
  in case Array.head rest of
    Nothing -> BRaw chunk
    Just first
      | startsWith "module "               first -> BHeader (parseHeader rest)
      | startsWith "import "               first -> BImports (Array.mapMaybe parseImport rest)
      | startsWith "foreign import data "  first -> BForeign (parseForeign true  first marginalia)
      | startsWith "foreign import "       first -> BForeign (parseForeign false first marginalia)
      | startsWith "class "                first -> BClass    { head: first, body: Array.drop 1 rest, marginalia }
      | startsWith "instance "             first -> BInstance { head: first, marginalia }
      | otherwise -> case parseValue rest of
          Just v  -> BValue (v { marginalia = marginalia })
          Nothing -> BRaw chunk

-- | Parse a single-line foreign import declaration.
-- |   `foreign import data X :: Kind` (isType: true)
-- |   `foreign import x :: Type`      (isType: false)
parseForeign :: Boolean -> String -> Array String -> ForeignBlock
parseForeign isType raw marginalia =
  let
    prefix = if isType then "foreign import data " else "foreign import "
    after = case String.stripPrefix (Pattern prefix) (String.trim raw) of
      Just s  -> s
      Nothing -> raw
  in case String.indexOf (Pattern " :: ") after of
    Just i ->
      { name: String.trim (String.take i after)
      , sig:  String.trim (String.drop (i + 4) after)
      , isType
      , marginalia
      }
    Nothing -> { name: String.trim after, sig: "", isType, marginalia }

peelComments :: Array String -> Tuple (Array String) (Array String)
peelComments chunk =
  let { init, rest } = Array.span isCommentLine chunk
  in Tuple init rest
  where
  isCommentLine l = startsWith "--" (String.trim l)

stripDocPrefix :: Array String -> Array String
stripDocPrefix = map step
  where
  step l =
    let t = String.trim l
    in firstJust
        [ String.stripPrefix (Pattern "-- | ") t
        , String.stripPrefix (Pattern "-- |")  t
        , String.stripPrefix (Pattern "-- ")   t
        , String.stripPrefix (Pattern "--")    t
        , Just t
        ]
        # fromMaybe t

firstJust :: forall a. Array (Maybe a) -> Maybe a
firstJust xs = Array.head (Array.mapMaybe identity xs)

-- ----------------------------------------------------------------------------
-- Header

parseHeader :: Array String -> Header
parseHeader lines =
  let
    joined = String.joinWith " " (map String.trim lines)
    afterModule = fromMaybe joined (String.stripPrefix (Pattern "module ") joined)
    name = String.trim (takeBefore [" ", "("] afterModule)
    exports = case sliceBalanced '(' ')' afterModule of
      Nothing -> []
      Just s  -> Array.filter (\x -> x /= "")
                   (splitTopLevel ',' s)
  in { name, exports }

-- ----------------------------------------------------------------------------
-- Imports

parseImport :: String -> Maybe ImportLine
parseImport raw = do
  rest <- String.stripPrefix (Pattern "import ") (String.trim raw)
  let Tuple qualified rest1 = case String.stripPrefix (Pattern "qualified ") rest of
        Just s  -> Tuple true s
        Nothing -> Tuple false rest
      items = sliceBalanced '(' ')' rest1
      openIdx = SCU.indexOf (Pattern "(") rest1
      beforeParen = case openIdx of
        Just i  -> String.take i rest1
        Nothing -> rest1
      afterParen = case openIdx of
        Just i ->
          let after = String.drop (i + 1) rest1
          in case sliceBalancedSpan '(' ')' after of
               Just len -> String.drop (len + 1) after
               Nothing  -> ""
        Nothing -> ""
      Tuple modPart aliasFromBefore = case String.indexOf (Pattern " as ") beforeParen of
        Just i  -> Tuple (String.take i beforeParen)
                         (Just (String.trim (String.drop (i + 4) beforeParen)))
        Nothing -> Tuple beforeParen Nothing
      aliasFromAfter = case String.indexOf (Pattern " as ") afterParen of
        Just i  -> Just (String.trim (String.drop (i + 4) afterParen))
        Nothing -> Nothing
      alias = firstJust [ aliasFromBefore, aliasFromAfter, Nothing ]
  pure { qualified, mod: String.trim modPart, alias, items }

-- ----------------------------------------------------------------------------
-- Value (sig + optional body, or body alone)
--
-- Three shapes handled:
--   (a) single-line sig:    `name :: type`     followed by body equation
--   (b) multi-line sig:     `name`             followed by indented `::` cont.
--                           `  :: forall ...`
--                           `  -> ...`
--                           `name args = ...`
--   (c) bare value:         `name args = body`  (no sig at all)

parseValue :: Array String -> Maybe ValueBlock
parseValue lines = case Array.head lines of
  Nothing -> Nothing
  Just first ->
    let trimmed = String.trim first in
    case String.indexOf (Pattern " :: ") trimmed of
      Just i ->
        Just { name: String.take i trimmed
             , sig:  Just (String.drop (i + 4) trimmed)
             , body: Array.drop 1 lines
             , marginalia: []
             }
      Nothing -> case parseMultilineSig lines of
        Just v  -> Just v
        Nothing ->
          case String.indexOf (Pattern " ") trimmed of
            Just i  -> Just { name: String.take i trimmed
                            , sig: Nothing
                            , body: lines
                            , marginalia: []
                            }
            Nothing -> Nothing

-- | Multi-line sig: bare identifier on line 0, indented continuation
-- | lines containing `::`, then the body equation at column 0 starting
-- | with the same identifier.
parseMultilineSig :: Array String -> Maybe ValueBlock
parseMultilineSig lines = do
  first <- Array.head lines
  let name = String.trim first
  if not (isBareIdent name)
    then Nothing
    else
      let
        rest = Array.drop 1 lines
        { init: sigLines, rest: bodyLines } = Array.span isIndented rest
        sig = String.trim (String.joinWith " " (map String.trim sigLines))
      in
        if Array.any containsSig sigLines && sig /= ""
          then Just { name, sig: Just sig, body: bodyLines, marginalia: [] }
          else Nothing
  where
  containsSig l = case String.indexOf (Pattern "::") l of
    Just _  -> true
    Nothing -> false

isBareIdent :: String -> Boolean
isBareIdent s = s /= "" && not (String.contains (Pattern " ") s)
                       && not (String.contains (Pattern "(") s)

isIndented :: String -> Boolean
isIndented line = case String.stripPrefix (Pattern " ") line of
  Just _  -> true
  Nothing -> case String.stripPrefix (Pattern "\t") line of
    Just _  -> true
    Nothing -> false

-- ----------------------------------------------------------------------------
-- Imports merging

mergeImports :: Array Block -> Array Block
mergeImports = go []
  where
  go acc xs = case Array.uncons xs of
    Nothing -> acc
    Just { head: BImports a, tail } ->
      let { gathered, rest } = takeImports a tail
      in go (Array.snoc acc (BImports gathered)) rest
    Just { head, tail } -> go (Array.snoc acc head) tail
  takeImports a xs = case Array.uncons xs of
    Just { head: BImports b, tail } -> takeImports (a <> b) tail
    _ -> { gathered: a, rest: xs }

-- ----------------------------------------------------------------------------
-- String helpers

startsWith :: String -> String -> Boolean
startsWith p s = case String.stripPrefix (Pattern p) s of
  Just _  -> true
  Nothing -> false

-- | Take everything before the earliest occurrence of any of the needle strings.
takeBefore :: Array String -> String -> String
takeBefore needles s =
  let positions = Array.mapMaybe (\n -> String.indexOf (Pattern n) s) needles
  in case Array.head (Array.sort positions) of
    Just i  -> String.take i s
    Nothing -> s

-- | Slice the substring between the first `open` char and its matching
-- | `close`, accounting for balanced nesting.
sliceBalanced :: Char -> Char -> String -> Maybe String
sliceBalanced open close s = do
  let chars = SCU.toCharArray s
  startIdx <- Array.findIndex (\c -> c == open) chars
  let after = Array.drop (startIdx + 1) chars
  endIdx <- findMatching close open after 1 0
  pure (SCU.fromCharArray (Array.take endIdx after))

-- | Like `sliceBalanced` but on `String` already advanced past the opening
-- | char, returning just the index of the matching closer.
sliceBalancedSpan :: Char -> Char -> String -> Maybe Int
sliceBalancedSpan open close s = do
  let chars = SCU.toCharArray s
  findMatching close open chars 1 0

findMatching :: Char -> Char -> Array Char -> Int -> Int -> Maybe Int
findMatching close open xs depth i = case Array.index xs i of
  Nothing -> Nothing
  Just c
    | c == close, depth == 1 -> Just i
    | c == close             -> findMatching close open xs (depth - 1) (i + 1)
    | c == open              -> findMatching close open xs (depth + 1) (i + 1)
    | otherwise              -> findMatching close open xs depth (i + 1)

-- | Split on a delimiter char only at top-level (depth 0 with respect to
-- | parens). Used so that exports/import-items split correctly even when
-- | individual entries contain nested parens like `(*>)`.
splitTopLevel :: Char -> String -> Array String
splitTopLevel delim s = go [] [] 0 (SCU.toCharArray s)
  where
  go acc cur depth xs = case Array.uncons xs of
    Nothing -> Array.snoc acc (SCU.fromCharArray cur # String.trim)
    Just { head, tail }
      | head == '(' -> go acc (Array.snoc cur head) (depth + 1) tail
      | head == ')' -> go acc (Array.snoc cur head) (depth - 1) tail
      | head == delim, depth == 0 ->
          go (Array.snoc acc (SCU.fromCharArray cur # String.trim)) [] 0 tail
      | otherwise   -> go acc (Array.snoc cur head) depth tail
