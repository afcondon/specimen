module Specimen.Preprocess
  ( glyphify
  ) where

import Prelude

import Data.Either (Either(..))
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)

-- | Replace ASCII operator sequences with their typographic Unicode
-- | equivalents. Run before block extraction.
-- |
-- | Substitutions are guarded with negative lookbehinds/lookaheads so
-- | composite operators (`<=<`, `>=>`, `<->`) keep their literal ASCII
-- | form and the font's contextual ligatures continue to fire on them:
-- |
-- |   `<=`   → `⇐`   only when not followed by `<`  (keep `<=<`)
-- |   `=>`   → `⇒`   only when not preceded by `>`  (keep `>=>`)
-- |   `->`   → `→`   only when not preceded by `<`  (keep `<->`)
-- |   `<-`   → `←`   always (no current conflict)
-- |   `forall` → `∀`
glyphify :: String -> String
glyphify =
  replaceAll (Pattern "forall") (Replacement "∀")
    >>> replaceArrow
    >>> replaceConstraint
    >>> replaceBindArrow
    >>> replaceSuperclass

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
