-- | Read a module's shape off its source text: its name, what it
-- | imports, how long it is, and where its top-level declarations
-- | start.
-- |
-- | This is deliberately a lexical skim, not a parse. The book's plates
-- | need a plausible declaration *size* distribution to pack into a
-- | circle, and the import graph needs edges; neither needs to be right
-- | about syntax the way `Specimen.Block` does. Anything that starts in
-- | column zero and isn't `module` or `import` anchors a declaration,
-- | and its span runs to the next such anchor.
module Specimen.Site.Harvest
  ( Decl
  , Harvested
  , harvest
  , moduleName
  , moduleNameOf
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))

import Specimen.Sig (isIdentChar)

-- | A top-level declaration anchor. `span` is the number of source
-- | lines it occupies, which becomes its weight in the module's plate.
type Decl =
  { id :: String
  , line :: Int
  , span :: Int
  }

type Harvested =
  { name :: String
  , source :: String
  , loc :: Int
  , decls :: Array Decl
  , imports :: Array String
  }

-- | Nothing when the text has no `module` header — a `.purs` file that
-- | isn't a module (or is empty) is skipped rather than guessed at.
harvest :: String -> Maybe Harvested
harvest source = do
  name <- moduleNameOf source
  pure
    { name
    , source
    , loc: Array.length (Array.filter (not <<< isBlank) sourceLines)
    , decls: spans (Array.length sourceLines) (anchors sourceLines)
    , imports: Array.mapMaybe importedModule sourceLines
    }
  where
  sourceLines = lines source

moduleNameOf :: String -> Maybe String
moduleNameOf = lines >>> Array.findMap (afterKeyword "module")

-- | Total version, for callers that would rather not marshal a `Maybe`.
-- | Text with no module header answers with the empty string.
moduleName :: String -> String
moduleName = moduleNameOf >>> fromMaybe ""

importedModule :: String -> Maybe String
importedModule = afterKeyword "import"

-- | The dotted module name following a leading keyword, if the line
-- | opens with exactly that keyword.
afterKeyword :: String -> String -> Maybe String
afterKeyword keyword line = do
  rest <- String.stripPrefix (Pattern (keyword <> " ")) line
  case takeWhile isModuleNameChar (String.trim rest) of
    "" -> Nothing
    name -> Just name

isModuleNameChar :: Char -> Boolean
isModuleNameChar c = isIdentChar c || c == '.'

-- ----------------------------------------------------------------------------
-- Declaration anchors

-- | Lines that begin a top-level declaration: column zero, starting on
-- | a letter or an open paren (an operator definition), and not one of
-- | the two header keywords. Consecutive lines that resolve to the same
-- | identifier are one declaration — a signature and its equations, or
-- | a multi-clause function.
anchors :: Array String -> Array { id :: String, line :: Int }
anchors =
  Array.mapWithIndex (\line text -> { line, text })
    >>> Array.filter (isAnchor <<< _.text)
    >>> map (\{ line, text } -> { id: anchorId text, line })
    >>> dedupeConsecutive

isAnchor :: String -> Boolean
isAnchor line =
  case SCU.charAt 0 line of
    Just c | isLetter c || c == '(' -> not (opensWith "module" || opensWith "import")
    _ -> false
  where
  opensWith kw = case String.stripPrefix (Pattern kw) line of
    Just rest -> maybe' true (not <<< isIdentChar) (SCU.charAt 0 rest)
    Nothing -> false

-- | The declared name, for grouping sibling lines. Falls back to a
-- | prefix of the line when nothing identifier-shaped is there.
anchorId :: String -> String
anchorId line =
  case takeWhile isIdentChar (fromMaybe line (String.stripPrefix (Pattern "(") line)) of
    "" -> String.take 8 line
    ident -> ident

dedupeConsecutive :: Array { id :: String, line :: Int } -> Array { id :: String, line :: Int }
dedupeConsecutive = Array.foldl step []
  where
  step acc a = case Array.last acc of
    Just prev | prev.id == a.id -> acc
    _ -> Array.snoc acc a

-- | Each declaration runs to the next one, and the last runs to the end
-- | of the file. A span is never less than one line.
spans :: Int -> Array { id :: String, line :: Int } -> Array Decl
spans total decls =
  Array.mapWithIndex
    (\i d ->
      let end = fromMaybe total (map _.line (Array.index decls (i + 1)))
      in { id: d.id, line: d.line, span: max 1 (end - d.line) })
    decls

-- ----------------------------------------------------------------------------
-- Helpers

lines :: String -> Array String
lines = String.split (Pattern "\n")

isBlank :: String -> Boolean
isBlank = String.trim >>> eq ""

isLetter :: Char -> Boolean
isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

takeWhile :: (Char -> Boolean) -> String -> String
takeWhile p = SCU.toCharArray >>> Array.takeWhile p >>> SCU.fromCharArray

maybe' :: forall a. Boolean -> (a -> Boolean) -> Maybe a -> Boolean
maybe' fallback f = case _ of
  Just a -> f a
  Nothing -> fallback
