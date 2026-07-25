-- | A package's release history, from the PureScript registry.
-- |
-- | This is the book's stability record: how often the library has been
-- | published, and how long ago the last one was. It is fetched once and
-- | cached beside the vendored sources, because a book gets rebuilt far
-- | more often than a library gets released.
-- |
-- | A local workspace has no release history, and a network that isn't
-- | there is not an error — the masthead simply loses its timeline.
module Specimen.Site.Registry
  ( History
  , releaseHistory
  , markMajors
  , majorOf
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Specimen.Site.Svg (Release)

type History =
  { releases :: Array Release
  , stableSince :: String
  }

-- | Every published version in date order, with the ones that crossed a
-- | major boundary marked so the sparkline can draw them taller.
-- |
-- | `Nothing` when the package has no registry presence, the fetch
-- | fails, or the metadata carries no usable dates.
releaseHistory :: String -> Aff (Maybe History)
releaseHistory name = do
  rows <- toAffE (fetchReleases name)
  case Array.last rows of
    Nothing -> pure Nothing
    Just newest -> do
      since <- liftEffect (monthYear newest.at)
      pure (Just { releases: markMajors rows, stableSince: since })

-- | A release is "major" when its leading number differs from the one
-- | before it. The first is never marked — there is nothing for it to
-- | be a break from.
markMajors :: Array Release -> Array Release
markMajors rows = Array.mapWithIndex mark rows
  where
  mark i r = r { major = case Array.index rows (i - 1) of
    Just previous -> majorOf previous.version /= majorOf r.version
    Nothing -> false }

-- | The leading number of a version string. Anything unparseable
-- | answers `""`, which compares equal to itself and so simply never
-- | reads as a major break.
majorOf :: String -> String
majorOf =
  SCU.toCharArray >>> Array.takeWhile isDigit >>> SCU.fromCharArray
  where
  isDigit c = c >= '0' && c <= '9'

-- | Published versions in date order. The JavaScript side owns the
-- | network, the on-disk cache, and date parsing; what comes back is
-- | already sorted and carries epoch millis.
foreign import fetchReleases :: String -> Effect (Promise (Array Release))

-- | "July 2026" — how the colophon says when.
foreign import monthYear :: Number -> Effect String
