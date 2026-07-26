-- | Tests for the decisions the book generator makes before it reads a
-- | single byte of source: which target it was given, which vendored
-- | package answers to a name, and — the one that bit — whether what is
-- | in the fetch cache is actually usable.
module Test.Site.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Specimen.Site.Sources
  ( Cache(..)
  , findVendored
  , inspectCache
  , isSourceFile
  , packageNameOfDirectory
  , plausiblePackageName
  , skipDirectory
  )

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = do
  describe "plausiblePackageName" do
    it "accepts registry names" do
      plausiblePackageName "aff" `shouldEqual` true
      plausiblePackageName "yoga-sql-types" `shouldEqual` true
      plausiblePackageName "d3" `shouldEqual` true
    it "rejects anything that was meant to be a path" do
      plausiblePackageName "./local" `shouldEqual` false
      plausiblePackageName "/abs/path" `shouldEqual` false
      plausiblePackageName "Capitalised" `shouldEqual` false
      plausiblePackageName "" `shouldEqual` false
    it "rejects a leading digit" do
      plausiblePackageName "2fast" `shouldEqual` false

  describe "packageNameOfDirectory" do
    it "drops the conventional repo prefix" do
      -- otherwise the seal reads "PURESCRIPT · PURESCRIPT HALOGEN WIDGETS"
      packageNameOfDirectory "purescript-halogen-widgets" `shouldEqual` "halogen-widgets"
    it "leaves anything else alone" do
      packageNameOfDirectory "my-app" `shouldEqual` "my-app"
      packageNameOfDirectory "purescript" `shouldEqual` "purescript"
      packageNameOfDirectory "purescript-" `shouldEqual` "purescript-"

  describe "findVendored" do
    it "finds the package at its version" do
      findVendored "maybe" [ "prelude-6.0.2", "maybe-6.0.0" ]
        `shouldEqual` Just { dir: "maybe-6.0.0", version: "6.0.0" }
    it "does not mistake a longer name for a version" do
      -- `aff-promise-4.0.0` starts with "aff-" too; only a leading digit
      -- after the hyphen marks the version, which is what tells them apart.
      findVendored "aff" [ "aff-promise-4.0.0" ] `shouldEqual` Nothing
      findVendored "aff" [ "aff-promise-4.0.0", "aff-8.0.0" ]
        `shouldEqual` Just { dir: "aff-8.0.0", version: "8.0.0" }
    it "is Nothing when nothing matches" do
      findVendored "lists" [ "maybe-6.0.0" ] `shouldEqual` Nothing

  describe "inspectCache" do
    let entries = [ "maybe-6.0.0" ]
        vendored = { dir: "maybe-6.0.0", version: "6.0.0" }

    it "reports a package that was never fetched" do
      inspectCache "lists" entries (const 12) `shouldEqual` NotFetched

    it "reports a vendored package with sources as usable" do
      inspectCache "maybe" entries (const 12) `shouldEqual` Usable vendored

    -- The regression this module exists for. A `spago fetch` that dies
    -- partway leaves `<name>-<version>/` on disk with an empty `src`.
    -- The old check asked only whether the directory existed, so the
    -- poisoned entry was treated as a hit forever after and every book
    -- built from it came out with no modules at all.
    it "treats a vendored package with no sources as incomplete" do
      inspectCache "maybe" entries (const 0) `shouldEqual` Incomplete vendored

  describe "isSourceFile" do
    it "takes .purs and nothing else" do
      isSourceFile "Data/Maybe.purs" `shouldEqual` true
      isSourceFile "Data/Maybe.js" `shouldEqual` false
      isSourceFile "purs.json" `shouldEqual` false
      isSourceFile "README.md" `shouldEqual` false

  describe "skipDirectory" do
    it "skips build output and dotfiles" do
      skipDirectory false "output" `shouldEqual` true
      skipDirectory false "node_modules" `shouldEqual` true
      skipDirectory false ".spago" `shouldEqual` true
      skipDirectory false "src" `shouldEqual` false
    it "skips test unless asked" do
      skipDirectory false "test" `shouldEqual` true
      skipDirectory true "test" `shouldEqual` false
