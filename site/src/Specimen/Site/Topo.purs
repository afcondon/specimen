-- | The import graph, and the layering the book reads by.
-- |
-- | A module's level is the longest path from a leaf: level 0 imports
-- | nothing else in the package, and the umbrella module sits highest.
-- | That ordering is the book's table of contents (left to right in the
-- | banner, top to bottom in the rail), so it has to be a total order
-- | even when the graph has a cycle — hence the visit guard, which
-- | pins a module at 0 while its own depth is being computed.
module Specimen.Site.Topo
  ( pruneImports
  , levels
  , accents
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, maximum)
import Data.Int (round, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- | Keep only edges that stay inside the package. Self-edges go, and so
-- | do edges into a bare re-export module named `Prelude` — those
-- | invert the layering, since the umbrella belongs at the top.
pruneImports
  :: forall r
   . Array { name :: String, imports :: Array String | r }
  -> Array { name :: String, imports :: Array String | r }
pruneImports mods = map prune mods
  where
  known = Set.fromFoldable (map _.name mods)
  prune m = m { imports = Array.filter (keep m.name) m.imports }
  keep self i = Set.member i known && i /= self && i /= "Prelude"

-- | Longest path from the leaves, by module name.
levels
  :: forall r
   . Array { name :: String, imports :: Array String | r }
  -> Map String Int
levels mods = foldl (\done m -> (depth done m.name).done) Map.empty mods
  where
  byName = Map.fromFoldable (map (\m -> Tuple m.name m.imports) mods)

  depth :: Map String Int -> String -> { done :: Map String Int, level :: Int }
  depth done name = case Map.lookup name done of
    Just level -> { done, level }
    Nothing -> case Map.lookup name byName of
      Nothing -> { done, level: 0 }
      Just imports ->
        let
          -- Pin at 0 before recursing: a cycle then resolves to the
          -- depth of its acyclic part rather than looping forever.
          stepped = foldl visit { done: Map.insert name 0 done, levels: [] } imports
          level = case maximum stepped.levels of
            Just deepest -> deepest + 1
            Nothing -> 0
        in
          { done: Map.insert name level stepped.done, level }

  visit acc i =
    let r = depth acc.done i
    in { done: r.done, levels: Array.snoc acc.levels r.level }

-- | A hue per module, spread across 0–300° in book order. Same grammar
-- | as The Prelude's beeswarm, so the two sit together on a shelf.
accents :: Array String -> Map String String
accents ordered = Map.fromFoldable (Array.mapWithIndex accent ordered)
  where
  n = Array.length ordered
  accent i name = Tuple name ("hsl(" <> show (hue i) <> ", 58%, 43%)")
  hue i
    | n <= 1 = 0
    | otherwise = round (toNumber i * 300.0 / toNumber (n - 1))
