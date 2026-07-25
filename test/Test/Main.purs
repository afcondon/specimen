-- | Golden tests over the whole pure pipeline: every checked-in example
-- | module is glyphified, classified into blocks, and typeset, and the
-- | result is compared against `test/golden`.
-- |
-- | The pipeline is pure and total — no fetch, no DOM — so this is a
-- | complete regression net over the part of Specimen that decides what
-- | the page looks like. Anything that changes the typography changes a
-- | golden file, and the diff is the review.
-- |
-- | Goldens are stored one tag per line. That is not the renderer's
-- | output shape (HTML is emitted unbroken, since whitespace between
-- | tags is significant in a `<pre>`); it is purely so `git diff` over
-- | an accepted change is readable. Both sides go through `breakTags`
-- | before comparison, so the reformatting cannot mask a difference.
-- |
-- | Re-accept the goldens after an intended change with:
-- |
-- | ```
-- | SPECIMEN_ACCEPT=1 spago test
-- | ```
module Test.Main (main) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, writeTextFile)
import Node.Process (lookupEnv)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Specimen.Block (extractBlocks)
import Specimen.Preprocess (glyphify)
import Specimen.Render (renderDocumentHtml)

examplesDir :: String
examplesDir = "public/examples"

goldenDir :: String
goldenDir = "test/golden"

main :: Effect Unit
main = do
  accept <- isJust <$> lookupEnv "SPECIMEN_ACCEPT"
  modules <- exampleModules
  runSpecAndExitProcess [ consoleReporter ] (spec accept modules)

spec :: Boolean -> Array String -> Spec Unit
spec accept modules =
  describe "renderDocumentHtml" $
    for_ modules \name ->
      it ("typesets " <> name <> " as recorded") do
        actual <- breakTags <$> liftEffect (typeset name)
        if accept then liftEffect (writeTextFile UTF8 (goldenPath name) actual)
        else do
          expected <- liftEffect (readTextFile UTF8 (goldenPath name))
          case firstDifference expected actual of
            Nothing -> pure unit
            Just d -> fail (report name d)

exampleModules :: Effect (Array String)
exampleModules =
  Array.sort
    <<< Array.mapMaybe (String.stripSuffix (Pattern ".purs"))
    <$> readdir examplesDir

typeset :: String -> Effect String
typeset name = do
  src <- readTextFile UTF8 (examplesDir <> "/" <> name <> ".purs")
  pure $ renderDocumentHtml
    { moduleSlug: name
    , source: "test-fixture"
    , blocks: extractBlocks (glyphify src)
    , notes: Map.empty
    }

goldenPath :: String -> String
goldenPath name = goldenDir <> "/" <> name <> ".html"

-- | One tag per line, for storage and for diffing.
breakTags :: String -> String
breakTags = String.replaceAll (Pattern "><") (Replacement ">\n<")

-- ----------------------------------------------------------------------------
-- Reporting
--
-- A typeset module is tens of thousands of characters; printing both
-- sides on mismatch buries the change. Report the first differing line
-- instead, which is the thing a reviewer needs to see.

type Difference =
  { line :: Int
  , expected :: Maybe String
  , actual :: Maybe String
  }

firstDifference :: String -> String -> Maybe Difference
firstDifference expected actual =
  case Array.findIndex identity (Array.zipWith (/=) expectedLines actualLines) of
    Just i -> Just (at i)
    Nothing
      | Array.length expectedLines == Array.length actualLines -> Nothing
      | otherwise -> Just (at (min (Array.length expectedLines) (Array.length actualLines)))
  where
  expectedLines = lines expected
  actualLines = lines actual
  at i =
    { line: i + 1
    , expected: Array.index expectedLines i
    , actual: Array.index actualLines i
    }

report :: String -> Difference -> String
report name { line, expected, actual } =
  String.joinWith "\n"
    [ name <> " differs from its golden at line " <> show line
    , "  expected: " <> clip expected
    , "  actual:   " <> clip actual
    , "  (re-accept with SPECIMEN_ACCEPT=1 spago test, then review the diff)"
    ]
  where
  clip = case _ of
    Nothing -> "<end of file>"
    Just s | String.length s > 160 -> String.take 160 s <> "…"
    Just s -> s

lines :: String -> Array String
lines = String.split (Pattern "\n")
