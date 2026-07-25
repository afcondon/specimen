-- | `specimen-site` — typeset a PureScript package as a book.
-- |
-- |     specimen-site <package-name | workspace-dir> [options]
-- |
-- |       --out, -o <dir>   where to write (default ./site/<package>)
-- |       --title <string>  book title (default: the package name)
-- |       --deck <string>   deck line under the title
-- |       --mark <glyph>    the formal mark at the seal's foot
-- |       --include-tests   include test/ modules from a local workspace
-- |
-- | A registry name is resolved by `spago fetch`; a directory is scanned
-- | for its sources. Every module is typeset at build time, so what comes
-- | out is a static site with no PureScript left in it.
module Specimen.Site.Main (main) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Specimen.Block (Block(..), extractBlocks)
import Specimen.Preprocess (glyphify)
import Specimen.Render (renderDocument)
import Specimen.Site.Book as Book
import Specimen.Site.Layout (BookModule, fromSources)
import Specimen.Site.Registry (releaseHistory)
import Specimen.Site.Sources (resolvePackage)
import Specimen.Site.Svg as Svg

main :: Effect Unit
main = launchAff_ do
  argv <- liftEffect args
  case Array.head argv of
    Nothing -> liftEffect (usage 1)
    Just "--help" -> liftEffect (usage 0)
    Just target -> build target argv

usage :: Int -> Effect Unit
usage code = do
  log "usage: specimen-site <package-name | workspace-dir> [-o dir] [--title T] [--deck D] [--mark G] [--include-tests]"
  exit code

build :: String -> Array String -> Aff Unit
build target argv = do
  pkg <- liftEffect (resolvePackage { target, includeTests })
  when (Array.null pkg.files) (liftEffect (die "no .purs files found"))

  history <- if pkg.local then pure Nothing else releaseHistory pkg.name
  sources <- liftEffect (traverse readText pkg.files)
  sidecars <- liftEffect (sidecarsFor pkg.files)
  today <- liftEffect isoDate
  now <- liftEffect epochMillis

  let
    book = fromSources sources
    title = fromMaybe pkg.name (option "--title" Nothing)
    outDir = fromMaybe (joinPath [ "site", pkg.name ]) (option "--out" (Just "-o"))
    sourceLabel = if pkg.local then pkg.name else pkg.name <> " v" <> pkg.version

    typeset = map (typesetModule sourceLabel sidecars) book.modules
    counts = tally (map _.blocks typeset)
    locTotal = Array.foldl (\n m -> n + m.loc) 0 book.modules
    seal = { name: pkg.name, mark, seal: book.seal }
    sealSvg = Svg.waxseal seal
    releases = maybe' [] _.releases history

    html = Book.page
      { facts:
          { title
          , deck: fromMaybe (defaultDeck (Array.length book.modules) counts.declarations)
              (option "--deck" Nothing)
          , sourceLabel
          , modules: Array.length book.modules
          , declarations: counts.declarations
          , lines: locTotal
          , stableSince: map _.stableSince history
          , today
          }
      , articles: map _.html typeset
      -- On the page the seal sits on the paper already, so it takes no
      -- backing of its own; the file and the favicon do.
      , seal: Svg.sealTree { opaque: false } seal
      , favicon: dataUri sealSvg
      , releases
      , now
      , payload: payloadJson
          { modules: book.modules
          , ffi: sidecars
          , labelSize: if Array.length book.modules >= 30 then 7.5 else 10.0
          }
      }

  liftEffect do
    makeDirectory outDir
    writeText (joinPath [ outDir, "index.html" ]) html
    writeText (joinPath [ outDir, "waxseal.svg" ]) sealSvg
    writeText (joinPath [ outDir, "banner.svg" ]) (Svg.banner book.modules)
    writeText (joinPath [ outDir, "book.json" ]) $ bookJson
      { name: pkg.name
      , version: pkg.version
      , title
      , modules: Array.length book.modules
      , decls: counts.declarations
      , loc: locTotal
      , layers: book.maxLevel
      , stableSince: maybe' "" _.stableSince history
      , releases
      }
    copyAssets outDir
    log (sourceLabel <> ": " <> show (Array.length book.modules) <> " modules, "
      <> show counts.declarations <> " declarations, " <> show locTotal
      <> " lines, layers 0.." <> show book.maxLevel)
    log ("unclassified blocks (BRaw): " <> show counts.unclassified)
    log ("→ " <> outDir)
  where
  option name short = optionAfter name short argv
  includeTests = Array.elem "--include-tests" argv
  mark = fromMaybe "λ" (optionAfter "--mark" Nothing argv)

-- ----------------------------------------------------------------------------
-- Articles

-- | A module's article, plus the blocks it was built from — the
-- | colophon's counts come from those, so they are carried rather than
-- | recomputed.
typesetModule
  :: String
  -> Array Sidecar
  -> BookModule
  -> { html :: Book.Html, blocks :: Array Block }
typesetModule sourceLabel sidecars m =
  { html:
      Book.article { slug: m.slug, name: m.name, accent: m.accent }
        (renderDocument
          { moduleSlug: m.name
          , source: sourceLabel
          , blocks
          , notes: Map.empty
          , foreignSidecar: if hasSidecar then Just m.slug else Nothing
          })
  , blocks
  }
  where
  blocks = extractBlocks (glyphify m.source)
  hasSidecar = Array.any (\s -> s.name == m.name) sidecars

-- | What the colophon counts. `BRaw` is the block classifier's shrug,
-- | reported so a regression in it shows up rather than passing quietly.
-- | Lines are not counted here — they are a property of the source, and
-- | `Specimen.Site.Harvest` already counted them.
type Tally =
  { declarations :: Int
  , unclassified :: Int
  }

tally :: Array (Array Block) -> Tally
tally = Array.foldl (Array.foldl count) { declarations: 0, unclassified: 0 }
  where
  count acc block = case block of
    BRaw _ -> acc { unclassified = acc.unclassified + 1 }
    _ | isDeclaration block -> acc { declarations = acc.declarations + 1 }
    _ -> acc

isDeclaration :: Block -> Boolean
isDeclaration = case _ of
  BValue _ -> true
  BData _ -> true
  BClass _ -> true
  BInstance _ -> true
  BForeign _ -> true
  BTypeAlias _ -> true
  _ -> false

defaultDeck :: Int -> Int -> String
defaultDeck modules declarations =
  show modules <> " modules · " <> show declarations
    <> " declarations · import order left to right · the swarm is the nav"

-- ----------------------------------------------------------------------------
-- Arguments

-- | The value following a flag, by long name or short alias.
optionAfter :: String -> Maybe String -> Array String -> Maybe String
optionAfter name short argv = do
  i <- Array.findIndex matches argv
  Array.index argv (i + 1)
  where
  matches a = a == name || Just a == short

maybe' :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe' fallback f = case _ of
  Just a -> f a
  Nothing -> fallback

-- ----------------------------------------------------------------------------
-- The host

-- | A foreign implementation shipped beside a module. Only JavaScript
-- | rides along in registry tarballs today; the other slots wait on the
-- | polyglot backends.
type Sidecar =
  { name :: String
  , slug :: String
  , javascript :: String
  }

foreign import args :: Effect (Array String)
foreign import exit :: Int -> Effect Unit
foreign import die :: forall a. String -> Effect a
foreign import readText :: String -> Effect String
foreign import writeText :: String -> String -> Effect Unit
foreign import makeDirectory :: String -> Effect Unit
foreign import copyAssets :: String -> Effect Unit
foreign import joinPath :: Array String -> String
foreign import isoDate :: Effect String
foreign import epochMillis :: Effect Number
foreign import dataUri :: String -> String

-- | Modules with a `.js` beside them, keyed for the page's script.
foreign import sidecarsFor :: Array String -> Effect (Array Sidecar)

-- | The two JSON documents the page and the shelf read. Both take plain
-- | records — no `Maybe` crosses this boundary, which is why the callers
-- | above resolve their optionals first.
foreign import payloadJson
  :: { modules :: Array BookModule, ffi :: Array Sidecar, labelSize :: Number } -> String

foreign import bookJson
  :: { name :: String
     , version :: String
     , title :: String
     , modules :: Int
     , decls :: Int
     , loc :: Int
     , layers :: Int
     , stableSince :: String
     , releases :: Array Svg.Release
     }
  -> String
