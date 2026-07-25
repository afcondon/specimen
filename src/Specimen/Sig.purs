-- | Signature analysis: where a type breaks, which variables it binds,
-- | and what colour each of those gets. Purely structural — nothing
-- | here knows about HTML. `Specimen.Html` turns the results into
-- | markup.
module Specimen.Sig
  ( Station
  , Connector(..)
  , segmentSig
  , forallVars
  , assignColors
  , shouldStack
  , palette
  , isIdentChar
  ) where

import Prelude

import Data.Array as Array
import Data.Char as Char
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))

data Connector
  = ConDot         -- terminates the ∀ binders
  | ConConstraint  -- ⇒
  | ConArrow       -- →
  | ConNone        -- last station, no trailing operator

type Station =
  { body :: String
  , connector :: Connector
  }

-- | Type-variable color palette. Hand-picked for cream paper background;
-- | each color reads as obviously colored without competing with the
-- | brick-red accent or appearing jewel-bright on the warm ground.
palette :: Array String
palette =
  [ "#1f5fa8" -- steel blue
  , "#b35900" -- burnt orange
  , "#2d7a3b" -- forest
  , "#7e3aa3" -- plum
  , "#0e7484" -- teal
  , "#5757a6" -- indigo
  , "#9c3d5e" -- raspberry
  , "#7a6017" -- olive/mustard
  ]

-- | Stack the type when its rendered length would overflow the column.
-- | Threshold tuned by eye against the current column width.
shouldStack :: String -> Boolean
shouldStack sig = String.length sig > 60

-- | Assign palette colors to type variables in declaration order so the
-- | first ∀-bound variable always gets the leading palette slot.
assignColors :: Array String -> Map String String
assignColors vars =
  let
    unique = Array.nub vars
    n = Array.length palette
    pairs = Array.mapWithIndex
      (\i v -> Tuple v (fromMaybe "#444" (Array.index palette (i `mod` n))))
      unique
  in Map.fromFoldable pairs

-- ----------------------------------------------------------------------------
-- ∀ extraction

-- | Extract variables bound by a leading `∀` / `forall` quantifier.
-- | Returns [] for sigs without a top-level quantifier — without CST we
-- | cannot reliably discover implicit binders, so we stay silent rather
-- | than risk colorizing a constructor name that happens to be one char.
-- |
-- | Kind-annotated binders like `(r ∷ Row Type)` are degenerated to the
-- | inner identifier.
forallVars :: String -> Array String
forallVars sig =
  let
    trimmed = String.trim sig
    after = case String.stripPrefix (Pattern "∀ ") trimmed of
      Just s  -> Just s
      Nothing -> String.stripPrefix (Pattern "forall ") trimmed
  in case after of
    Nothing -> []
    Just s ->
      let chars = SCU.toCharArray s
      in case findForallDot chars of
        Nothing -> []
        Just i ->
          String.split (Pattern " ") (SCU.fromCharArray (Array.take i chars))
            # Array.mapMaybe extractBinder

extractBinder :: String -> Maybe String
extractBinder raw =
  let
    trimmed = String.trim raw
    stripped = fromMaybe trimmed (String.stripPrefix (Pattern "(") trimmed)
    ident = takeWhileIdent stripped
  in if ident == "" then Nothing else Just ident

takeWhileIdent :: String -> String
takeWhileIdent s = SCU.fromCharArray (Array.takeWhile isIdentChar (SCU.toCharArray s))

isIdentChar :: Char -> Boolean
isIdentChar c =
  let code = Char.toCharCode c
  in (code >= 0x30 && code <= 0x39)  -- 0-9
  || (code >= 0x41 && code <= 0x5A)  -- A-Z
  || (code >= 0x61 && code <= 0x7A)  -- a-z
  || code == 0x27                     -- '
  || code == 0x5F                     -- _

-- | Find the dot that terminates a `∀` binder list.
-- | A forall-dot is followed by whitespace (or end of string); a
-- | module-qualifier dot like `RL.RowToList` is followed by an identifier
-- | character, which lets us discriminate without depth tracking through
-- | qualified names.
findForallDot :: Array Char -> Maybe Int
findForallDot xs = go 0 0
  where
  go depth i = case Array.index xs i of
    Nothing -> Nothing
    Just c
      | c == '.' && depth == 0 && nextIsSpaceOrEnd xs i -> Just i
      | c == '(' || c == '[' || c == '{' -> go (depth + 1) (i + 1)
      | c == ')' || c == ']' || c == '}' -> go (depth - 1) (i + 1)
      | otherwise -> go depth (i + 1)

  nextIsSpaceOrEnd ys i = case Array.index ys (i + 1) of
    Nothing -> true
    Just c  -> isSpace c

isSpace :: Char -> Boolean
isSpace c = c == ' ' || c == '\t' || c == '\n'

-- ----------------------------------------------------------------------------
-- Segmenter

-- | Split a sig string into stations: each station is a chunk of type
-- | text plus the operator that terminates it (or ConNone for the last).
-- | Handles the leading `∀ … .` as one station; then walks left-to-right
-- | with paren depth tracking, splitting on top-level `→` / `⇒`.
segmentSig :: String -> Array Station
segmentSig sig =
  let
    trimmed = String.trim sig
    Tuple pre rest = peelForall trimmed
  in pre <> walkSegments rest

peelForall :: String -> Tuple (Array Station) String
peelForall s = case String.stripPrefix (Pattern "∀ ") s of
  Just inner -> peel "∀" inner
  Nothing -> case String.stripPrefix (Pattern "forall ") s of
    Just inner -> peel "forall" inner
    Nothing -> Tuple [] s
  where
  peel quant inner =
    let chars = SCU.toCharArray inner
    in case findForallDot chars of
      Nothing -> Tuple [] s
      Just i ->
        let
          vars = String.trim (SCU.fromCharArray (Array.take i chars))
          rest = String.trim (SCU.fromCharArray (Array.drop (i + 1) chars))
          body = quant <> " " <> vars
        in Tuple [ { body, connector: ConDot } ] rest

walkSegments :: String -> Array Station
walkSegments input = go [] [] 0 (SCU.toCharArray input)
  where
  go acc cur depth xs = case Array.uncons xs of
    Nothing ->
      let body = String.trim (SCU.fromCharArray cur)
      in if body == "" then acc
         else Array.snoc acc { body, connector: ConNone }
    Just { head, tail }
      | head == '(' || head == '[' || head == '{' ->
          go acc (Array.snoc cur head) (depth + 1) tail
      | head == ')' || head == ']' || head == '}' ->
          go acc (Array.snoc cur head) (depth - 1) tail
      | depth == 0 && head == '\x2192' ->  -- →
          flush acc cur ConArrow tail depth
      | depth == 0 && head == '\x21D2' ->  -- ⇒
          flush acc cur ConConstraint tail depth
      | otherwise -> go acc (Array.snoc cur head) depth tail

  flush acc cur conn tail depth =
    let body = String.trim (SCU.fromCharArray cur)
    in go (Array.snoc acc { body, connector: conn }) [] depth tail
