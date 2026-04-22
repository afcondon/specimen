module Specimen.Preprocess
  ( glyphify
  ) where

import Prelude

import Data.Either (Either(..))
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)

-- | Normalize then glyphify.
-- |
-- | Step 1 (normalize) — convert PureScript's Unicode token forms back
-- | to ASCII so the downstream block parser sees a consistent shape no
-- | matter how the source author chose to write them. PureScript accepts
-- | both `::` and `∷`, both `forall` and `∀`, etc.; without this step,
-- | a Unicode-style source (e.g. Variant) parses as opaque BRaw.
-- |
-- | Step 2 (substitute) — replace ASCII operator sequences with their
-- | typographic Unicode equivalents for display. Substitutions are
-- | guarded with negative lookbehinds/lookaheads so composite operators
-- | (`<=<`, `>=>`, `<->`) keep their literal ASCII form and the font's
-- | contextual ligatures continue to fire on them:
-- |
-- |   `<=`   → `⇐`   only when not followed by `<`  (keep `<=<`)
-- |   `=>`   → `⇒`   only when not preceded by `>`  (keep `>=>`)
-- |   `->`   → `→`   only when not preceded by `<`  (keep `<->`)
-- |   `<-`   → `←`   always (no current conflict)
-- |   `forall` → `∀`
glyphify :: String -> String
glyphify =
  normalize
    >>> replaceAll (Pattern "forall") (Replacement "∀")
    >>> replaceArrow
    >>> replaceConstraint
    >>> replaceBindArrow
    >>> replaceSuperclass

-- | Convert PureScript's Unicode token forms to ASCII so the parser sees
-- | a consistent shape. The display layer re-substitutes back via the
-- | step-2 rules above, so output is identical regardless of input form.
normalize :: String -> String
normalize =
  replaceAll (Pattern "∷") (Replacement "::")
    >>> replaceAll (Pattern "∀") (Replacement "forall")
    >>> replaceAll (Pattern "→") (Replacement "->")
    >>> replaceAll (Pattern "←") (Replacement "<-")
    >>> replaceAll (Pattern "⇒") (Replacement "=>")
    >>> replaceAll (Pattern "⇐") (Replacement "<=")

replaceArrow :: String -> String
replaceArrow s = case regex "(?<!<)->" global of
  Right r -> replace r "→" s
  Left _  -> s

replaceConstraint :: String -> String
replaceConstraint s = case regex "(?<!>)=>" global of
  Right r -> replace r "⇒" s
  Left _  -> s

replaceBindArrow :: String -> String
replaceBindArrow s = replaceAll (Pattern "<-") (Replacement "←") s

replaceSuperclass :: String -> String
replaceSuperclass s = case regex "<=(?!<)" global of
  Right r -> replace r "⇐" s
  Left _  -> s
