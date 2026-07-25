-- | Everything the book's identity kit needs, computed from the
-- | harvested modules: layering, accents, disc radii, per-module plates,
-- | the two swarm arrangements, and the seal.
-- |
-- | This is the whole of what used to be d3's job in the site generator.
module Specimen.Site.Layout
  ( BookModule
  , Book
  , Levelled
  , layout
  , fromSources
  , slugify
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (maximum)
import Data.Int (round, toNumber)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Number (sin, sqrt)
import Data.String as String
import Data.String.Pattern (Pattern(..), Replacement(..))

import Specimen.Site.Beeswarm (Config, Placed, defaultConfig, settle)
import Specimen.Site.Harvest (Decl, Harvested, harvest)
import Specimen.Site.Pack (Disc, SealCircle, plate, waxseal)
import Specimen.Site.Topo (accents, levels, pruneImports)

-- | A harvested module once its place in the import order is known.
type Levelled =
  { name :: String
  , source :: String
  , loc :: Int
  , decls :: Array Decl
  , imports :: Array String
  , level :: Int
  }

type BookModule =
  { name :: String
  , slug :: String
  , source :: String
  , loc :: Int
  , decls :: Array Decl
  , imports :: Array String -- slugs, in-package edges only
  , level :: Int
  , accent :: String
  , rA :: Number -- banner radius
  , rB :: Number -- rail radius
  , plate :: Array Disc
  , ax :: Number -- banner position, normalised 0..1
  , ay :: Number
  , bx :: Number -- rail position, px within the rail column
  , by :: Number
  }

type Book =
  { modules :: Array BookModule
  , maxLevel :: Int
  , seal :: Array SealCircle
  }

-- ----------------------------------------------------------------------------
-- Geometry
--
-- Layout A is the hero banner: a wide band, import order left to right.
-- Layout B is the rail: a tall narrow column in the left margin. The
-- page interpolates between them as the reader scrolls, so both are
-- computed here and shipped together.

bannerWidth :: Number
bannerWidth = 1600.0

bannerHeight :: Number
bannerHeight = 560.0

railWidth :: Number
railWidth = 176.0

railHeight :: Number
railHeight = 900.0

railMargin :: Number
railMargin = 40.0

sealRadius :: Number
sealRadius = 190.0

-- | Harvest raw module sources and lay the book out in one call.
-- | Sources without a module header are skipped.
fromSources :: Array String -> Book
fromSources = Array.mapMaybe harvest >>> layout

layout :: Array Harvested -> Book
layout harvested =
  { modules, maxLevel, seal: waxseal sealRadius ordered }
  where
  pruned = pruneImports harvested
  levelOf = levels pruned

  -- Book order: by layer, then alphabetically. Everything downstream —
  -- accent hue, article order, the swarm's node indices — reads from it.
  ordered = Array.sortBy byLayerThenName (map levelled pruned)
  levelled m =
    { name: m.name
    , source: m.source
    , loc: m.loc
    , decls: m.decls
    , imports: m.imports
    , level: fromMaybe 0 (Map.lookup m.name levelOf)
    }

  count = Array.length ordered
  maxLevel = fromMaybe 0 (maximum (map _.level ordered))
  accentOf = accents (map _.name ordered)

  -- Small books get bigger plates: three lonely islands read better as
  -- a group than as three specks in a wide band.
  plateScale = if count < 12 then 5.0 else 3.4

  sized = map (\m -> { m, rA: radiusA m.loc, rB: radiusB m.loc }) ordered
  radiusA loc = max 6.0 (plateScale * sqrt (toNumber loc))
  radiusB loc = max 3.5 (1.15 * sqrt (toNumber loc))

  bannerPlaces = settle bannerForces (map bannerNode sized)
  railPlaces = settle railForces (map railNode sized)

  bannerNode { m, rA } =
    { x: bannerTargetX maxLevel count m.level
    , y: bannerHeight / 2.0 + sin (toNumber (String.length m.name) * 7.3) * 150.0
    , targetX: bannerTargetX maxLevel count m.level
    , targetY: bannerHeight / 2.0
    , radius: rA + 7.0
    }

  railNode { m, rB } =
    { x: railWidth / 2.0 + sin (toNumber (String.length m.name) * 3.1) * 30.0
    , y: railTargetY maxLevel m.level
    , targetX: railWidth / 2.0
    , targetY: railTargetY maxLevel m.level
    , radius: rB + 2.5
    }

  modules = Array.mapWithIndex assemble sized

  assemble i { m, rA, rB } =
    { name: m.name
    , slug: slugify m.name
    , source: m.source
    , loc: m.loc
    , decls: m.decls
    , imports: map slugify m.imports
    , level: m.level
    , accent: fromMaybe "" (Map.lookup m.name accentOf)
    , rA
    , rB
    , plate: plate rA m.decls
    , ax: round4 (coord bannerPlaces i _.x / bannerWidth)
    , ay: round4 (coord bannerPlaces i _.y / bannerHeight)
    , bx: round1 (coord railPlaces i _.x)
    , by: round1 (coord railPlaces i _.y)
    }

  coord :: Array Placed -> Int -> (Placed -> Number) -> Number
  coord places i f = fromMaybe 0.0 (map f (Array.index places i))

byLayerThenName :: Levelled -> Levelled -> Ordering
byLayerThenName a b = case compare a.level b.level of
  EQ -> compare a.name b.name
  other -> other

-- | The banner pulls hard along the import axis and only gently towards
-- | the vertical centre, so layers read as columns and collision does
-- | the rest.
bannerForces :: Config
bannerForces = defaultConfig
  { strengthX = 0.7, strengthY = 0.055, collideIterations = 4 }

-- | The rail is the same idea rotated: hard onto the layer's row, weak
-- | centring across the narrow column.
railForces :: Config
railForces = defaultConfig
  { strengthX = 0.08, strengthY = 0.85, collideIterations = 4 }

-- | The band is clamped and centred rather than always spanning the
-- | full width, so a small package reads as a group.
bannerSpan :: Int -> Int -> Number
bannerSpan maxLevel count =
  min (bannerWidth - 180.0)
    (max 420.0 (max (toNumber maxLevel * 300.0) (sqrt (toNumber count) * 340.0)))

bannerTargetX :: Int -> Int -> Int -> Number
bannerTargetX maxLevel count level =
  let
    span = bannerSpan maxLevel count
    margin = (bannerWidth - span) / 2.0
    step = if maxLevel == 0 then 0.0 else span / toNumber maxLevel
  in
    margin + toNumber level * step

railTargetY :: Int -> Int -> Number
railTargetY maxLevel level =
  let step = if maxLevel == 0 then 0.0 else (railHeight - 2.0 * railMargin) / toNumber maxLevel
  in railMargin + toNumber level * step

-- ----------------------------------------------------------------------------
-- Helpers

-- | Dotted module name → DOM-safe id fragment.
slugify :: String -> String
slugify = String.replaceAll (Pattern ".") (Replacement "-")

round1 :: Number -> Number
round1 = roundTo 10.0

round4 :: Number -> Number
round4 = roundTo 10000.0

roundTo :: Number -> Number -> Number
roundTo scale x = toNumber (round (x * scale)) / scale
