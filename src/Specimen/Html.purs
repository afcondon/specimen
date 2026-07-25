-- | The presentation layer: decorated source text carried as **tokens**
-- | rather than as pre-escaped HTML strings.
-- |
-- | Specimen applies four decorations to fragments of code — glyph
-- | emphasis, type-variable colour, quieted keywords, and soft-break
-- | opportunities inside long identifiers. Each was once a regex
-- | substitution over an already-escaped `String`, which put the
-- | escaping and the markup in the same flat value: every call site had
-- | to remember to `escape` first, every pass had to avoid matching
-- | inside markup an earlier pass had emitted, and nothing in the types
-- | distinguished "source text" from "HTML".
-- |
-- | Here a decoration is a pass over an `Array Token`. A pass rewrites
-- | only `Plain` runs, so passes compose in any order and cannot damage
-- | each other's output, and escaping happens exactly once — in
-- | `toHtml`, where Halogen builds the text nodes.
module Specimen.Html
  ( Token(..)
  , onPlain
  , runs
  , glyphs
  , vars
  , keyword
  , fixity
  , softBreaks
  , toHtml
  , plain
  , decorated
  , glyphed
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))

import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP

import Specimen.Sig (isIdentChar)

-- | A run of source text, together with the decoration claiming it.
-- | `Plain` is undecorated source — the only case the passes below will
-- | rewrite, which is what keeps them commutative.
data Token
  = Plain String
  -- | Text wrapped in a `<span>` carrying one class: the quieted-keyword
  -- | and emphasised-glyph treatments.
  | Marked ClassName String
  -- | A type variable, in the colour `Specimen.Sig` assigned it.
  | Var String String
  -- | A `<wbr>`: a place the browser may break a long identifier.
  | Break

derive instance Eq Token

-- | Lift a pass over source text to a pass over tokens. Only `Plain`
-- | runs are offered to `f`; everything a previous pass has already
-- | claimed travels through untouched.
onPlain :: (String -> Array Token) -> Array Token -> Array Token
onPlain f = Array.concatMap case _ of
  Plain s -> f s
  claimed -> [ claimed ]

-- | Split a string into maximal runs of characters that agree on `p`,
-- | tagged with the value of `p` over the run. The shared primitive
-- | under `glyphs` and `vars` — both are "find the runs that are one
-- | kind of thing, leave the rest alone".
runs :: (Char -> Boolean) -> String -> Array (Tuple Boolean String)
runs p =
  SCU.toCharArray
    >>> Array.groupBy (\a b -> p a == p b)
    >>> map (\g -> Tuple (p (NEA.head g)) (SCU.fromCharArray (NEA.toArray g)))

-- | The single token every undecorated string starts as.
plain :: String -> Array Token
plain s = [ Plain s ]

-- ----------------------------------------------------------------------------
-- Passes

-- | Emphasise the substituted Unicode glyphs. `Specimen.Preprocess`
-- | swaps `->` for `→` and friends; CSS bumps their size to match the
-- | visual weight of the JetBrains Mono ligatures around them.
glyphs :: String -> Array Token
glyphs = runs isGlyph >>> map case _ of
  Tuple true  t -> Marked glyphClass t
  Tuple false t -> Plain t

isGlyph :: Char -> Boolean
isGlyph c = c == '→' || c == '←' || c == '⇒' || c == '⇐' || c == '∀'

glyphClass :: ClassName
glyphClass = ClassName "g"

-- | Colour each occurrence of a known type variable. Runs of identifier
-- | characters are looked up whole, so `r` never matches inside `r'` or
-- | `Row` — the boundary condition the old lookbehind regex encoded.
vars :: Map String String -> String -> Array Token
vars colors
  | Map.isEmpty colors = plain
  | otherwise = runs isIdentChar >>> map case _ of
      Tuple true t | Just colour <- Map.lookup t colors -> Var colour t
      Tuple _ t -> Plain t

-- | Quiet a leading declaration keyword. The left-gutter label already
-- | names the block kind, so `class` / `instance` / `data` in the code
-- | line is set in the margin colour. Candidates are tried longest
-- | first and must end on a word boundary, so a value named `datum`
-- | keeps its ink.
keyword :: Array String -> String -> Array Token
keyword candidates s =
  case Array.head (Array.mapMaybe (\kw -> splitKeyword kw s) candidates) of
    Just { kw, rest } -> [ Marked keywordClass kw, Plain rest ]
    Nothing -> plain s

splitKeyword :: String -> String -> Maybe { kw :: String, rest :: String }
splitKeyword kw s = do
  rest <- String.stripPrefix (Pattern kw) s
  if boundary (SCU.charAt 0 rest) then Just { kw, rest } else Nothing
  where
  boundary = maybe true (not <<< isIdentChar)

keywordClass :: ClassName
keywordClass = ClassName "kw"

-- | Quiet the fixity machinery: the `infixr 9` run and the ` as `
-- | connective drop to the margin colour, leaving the target and the
-- | operator in ink.
fixity :: String -> Array Token
fixity = leadingFixity >>> onPlain asConnective
  where
  asConnective s = case String.split (Pattern " as ") s of
    [ before, after ] -> [ Plain before, Marked keywordClass " as ", Plain after ]
    _ -> plain s

-- | Peel `infixr 9 ` / `infixl 4 type ` off the front. Hand-walked
-- | rather than matched, because the arity is a number and the optional
-- | `type ` keyword makes the shape awkward to express as a candidate
-- | list.
leadingFixity :: String -> Array Token
leadingFixity s = case String.split (Pattern " ") s of
  parts | Just fix <- Array.index parts 0
        , isInfixKeyword fix
        , Just arity <- Array.index parts 1
        , allDigits arity ->
      let
        typed = Array.index parts 2 == Just "type"
        taken = if typed then 3 else 2
        run = String.joinWith " " (Array.take taken parts) <> " "
      in
        [ Marked keywordClass run
        , Plain (String.joinWith " " (Array.drop taken parts))
        ]
  _ -> plain s

isInfixKeyword :: String -> Boolean
isInfixKeyword s = s == "infix" || s == "infixl" || s == "infixr"

allDigits :: String -> Boolean
allDigits s =
  not (String.null s)
    && Array.all (\c -> c >= '0' && c <= '9') (SCU.toCharArray s)

-- | Offer break opportunities at an identifier's morpheme seams — the
-- | lower→Upper camelCase transitions and after underscores — so a long
-- | export name wraps like a hyphenated word, never mid-morpheme.
softBreaks :: String -> Array Token
softBreaks = SCU.toCharArray >>> go [] []
  where
  go acc cur chars = case Array.uncons chars of
    Nothing -> flush acc cur
    Just { head: c, tail }
      | seam (Array.last cur) c -> go (Array.snoc (flush acc cur) Break) [ c ] tail
      | c == '_' -> go (Array.snoc (flush acc (Array.snoc cur c)) Break) [] tail
      | otherwise -> go acc (Array.snoc cur c) tail

  flush acc cur =
    if Array.null cur then acc
    else Array.snoc acc (Plain (SCU.fromCharArray cur))

  seam prev c = isUpper c && maybe false isLowerOrDigit prev

isUpper :: Char -> Boolean
isUpper c = c >= 'A' && c <= 'Z'

isLowerOrDigit :: Char -> Boolean
isLowerOrDigit c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

-- ----------------------------------------------------------------------------
-- Emission

-- | Build the Halogen nodes. This is the only place text becomes HTML,
-- | and Halogen escapes it — for the live component and, via
-- | `Halogen.VDom.DOM.StringRenderer`, for the static site alike.
toHtml :: forall w i. Array Token -> Array (HH.HTML w i)
toHtml = map case _ of
  Plain s -> HH.text s
  Marked cls s -> HH.span [ HP.class_ cls ] [ HH.text s ]
  Var colour s ->
    HH.var
      [ HP.class_ (ClassName "sig-var"), HP.style ("--vc:" <> colour) ]
      [ HH.text s ]
  Break -> HH.wbr []

-- ----------------------------------------------------------------------------
-- Common compositions

-- | A type expression: variables coloured, glyphs emphasised. The
-- | composition Specimen reaches for most.
decorated :: forall w i. Map String String -> String -> Array (HH.HTML w i)
decorated colors = plain >>> onPlain (vars colors) >>> onPlain glyphs >>> toHtml

-- | Code text with glyphs emphasised but no variable colouring.
glyphed :: forall w i. String -> Array (HH.HTML w i)
glyphed = plain >>> onPlain glyphs >>> toHtml
