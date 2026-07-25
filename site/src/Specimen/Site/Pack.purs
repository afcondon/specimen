-- | The book's two circle-packs, both from
-- | `DataViz.Layout.Hierarchy.Pack` — the Hylograph port of the
-- | front-chain algorithm. Nothing here calls d3.
-- |
-- |   · the **plate**: one module's declarations packed into the disc
-- |     that stands for the module in the banner and the rail. Each
-- |     declaration's area is its source span, so a module reads as its
-- |     own distribution of large and small definitions.
-- |
-- |   · the **waxseal**: the whole package's namespace tree packed into
-- |     the seal that closes the colophon and serves as the favicon.
module Specimen.Site.Pack
  ( Disc
  , SealCircle
  , plate
  , waxseal
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Pattern (Pattern(..))

import DataViz.Layout.Hierarchy.Pack (HierarchyData(..), PackNode(..), hierarchy, pack)

-- | A packed circle, positioned relative to the centre of its parent.
type Disc =
  { dx :: Number
  , dy :: Number
  , r :: Number
  }

-- | A circle of the seal. Containers are drawn as hairlines and leaves
-- | as solid ink, so the namespace structure reads as nesting.
type SealCircle =
  { x :: Number
  , y :: Number
  , r :: Number
  , container :: Boolean
  }

-- ----------------------------------------------------------------------------
-- Plate

-- | Pack a module's declarations into a disc of the given radius.
plate :: forall r. Number -> Array { span :: Int | r } -> Array Disc
plate radius decls =
  map recentre (leaves (pack config (hierarchy tree)))
  where
  config =
    { size: { width: 2.0 * radius, height: 2.0 * radius }
    , padding: 1.4
    , radius: Nothing
    }

  tree = HierarchyData
    { data_: unit
    , value: Nothing
    , children: Just (map declLeaf decls)
    }

  declLeaf d = HierarchyData
    { data_: unit
    , value: Just (toNumber d.span)
    , children: Nothing
    }

  -- The pack is laid out in a 2r box; the plate is drawn centred on the
  -- module's position, so shift the origin to the middle.
  recentre (PackNode n) =
    { dx: round1 (n.x - radius)
    , dy: round1 (n.y - radius)
    , r: round1 n.r
    }

-- ----------------------------------------------------------------------------
-- Waxseal

-- | Pack the package's namespace tree — `Data.Maybe.First` nests inside
-- | `Data.Maybe` inside `Data` — weighted by lines of code.
waxseal :: forall r. Number -> Array { name :: String, loc :: Int | r } -> Array SealCircle
waxseal radius mods =
  Array.mapMaybe toCircle (descendants (pack config (hierarchy (namespaceTree mods))))
  where
  config =
    { size: { width: 2.0 * radius, height: 2.0 * radius }
    , padding: 5.0
    , radius: Nothing
    }

  -- Depth 0 is the package itself; drawing it would just be a circle
  -- around everything, which the seal's own rim already is.
  toCircle (PackNode n)
    | n.depth == 0 = Nothing
    | otherwise = Just
        { x: n.x, y: n.y, r: n.r, container: not (Array.null n.children) }

-- | A node of the namespace tree under construction.
newtype Branch = Branch
  { key :: String
  , loc :: Int
  , children :: Array Branch
  }

emptyBranch :: Branch
emptyBranch = Branch { key: "", loc: 0, children: [] }

-- | Build the namespace tree from dotted module names.
namespaceTree :: forall r. Array { name :: String, loc :: Int | r } -> HierarchyData Unit
namespaceTree = foldl insert emptyBranch >>> toHierarchy
  where
  insert root m = insertAt (String.split (Pattern ".") m.name) m.loc root

toHierarchy :: Branch -> HierarchyData Unit
toHierarchy branch@(Branch b)
  | Array.null b.children =
      HierarchyData { data_: unit, value: Just (toNumber b.loc), children: Nothing }
  | otherwise =
      HierarchyData
        { data_: unit
        , value: Nothing
        , children: Just (map toHierarchy (hoistSelf branch))
        }

-- | A module that is also a namespace parent — `Data.Maybe` with
-- | `Data.Maybe.First` under it — must contribute a drawable leaf of
-- | its own, not just weight on a container, or its code vanishes from
-- | the seal. It gets an extra leaf child carrying its own lines.
hoistSelf :: Branch -> Array Branch
hoistSelf (Branch b)
  | b.loc > 0 = Array.snoc b.children (Branch { key: b.key <> " (self)", loc: b.loc, children: [] })
  | otherwise = b.children

insertAt :: Array String -> Int -> Branch -> Branch
insertAt path loc (Branch b) = case Array.uncons path of
  Nothing -> Branch (b { loc = loc })
  Just { head, tail } ->
    case Array.findIndex (\(Branch c) -> c.key == head) b.children of
      Just i ->
        let existing = fromMaybe emptyBranch (Array.index b.children i)
        in Branch b
             { children = fromMaybe b.children
                 (Array.updateAt i (insertAt tail loc existing) b.children)
             }
      Nothing ->
        Branch b
          { children = Array.snoc b.children
              (insertAt tail loc (Branch { key: head, loc: 0, children: [] }))
          }

-- ----------------------------------------------------------------------------
-- PackNode traversal
--
-- The library hands back a tree; the book wants flat arrays out of it.

leaves :: forall a. PackNode a -> Array (PackNode a)
leaves node@(PackNode n)
  | Array.null n.children = [ node ]
  | otherwise = Array.concatMap leaves n.children

descendants :: forall a. PackNode a -> Array (PackNode a)
descendants node@(PackNode n) =
  Array.cons node (Array.concatMap descendants n.children)

-- | The generated SVG carries these numbers as attribute text; one
-- | decimal place is past the resolution of any screen the book is read
-- | on, and keeps the payload small.
round1 :: Number -> Number
round1 x = toNumber (round (x * 10.0)) / 10.0
