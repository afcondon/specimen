module Specimen.Block
  ( Block(..)
  , Header
  , ImportLine
  , ClassBlock
  , InstanceBlock
  , ValueBlock
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

data Block
  = BHeader   Header
  | BImports  (Array ImportLine)
  | BClass    ClassBlock
  | BInstance InstanceBlock
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

groupChunks :: Array String -> Array (Array String)
groupChunks lines = go [] [] lines
  where
  go acc cur xs = case Array.uncons xs of
    Nothing -> if Array.null cur then acc else Array.snoc acc cur
    Just { head, tail }
      | isBlank head -> if Array.null cur
                          then go acc [] tail
                          else go (Array.snoc acc cur) [] tail
      | otherwise    -> go acc (Array.snoc cur head) tail

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
      | startsWith "module "   first -> BHeader (parseHeader rest)
      | startsWith "import "   first -> BImports (Array.mapMaybe parseImport rest)
      | startsWith "class "    first -> BClass    { head: first, body: Array.drop 1 rest, marginalia }
      | startsWith "instance " first -> BInstance { head: first, marginalia }
      | otherwise -> case parseValue rest of
          Just v  -> BValue (v { marginalia = marginalia })
          Nothing -> BRaw chunk

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
      Nothing ->
        case String.indexOf (Pattern " ") trimmed of
          Just i  -> Just { name: String.take i trimmed
                          , sig: Nothing
                          , body: lines
                          , marginalia: []
                          }
          Nothing -> Nothing

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
