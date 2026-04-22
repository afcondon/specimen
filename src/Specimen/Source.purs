module Specimen.Source
  ( fetchText
  , queryParam
  ) where

import Prelude
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import _fetchText  :: String -> Effect (Promise String)
foreign import _queryParam :: String -> Effect (Nullable String)

-- | Fetch a URL as plain text. Throws via the underlying Aff if the request fails.
fetchText :: String -> Aff String
fetchText url = toAffE (_fetchText url)

-- | Read a URL query parameter from `window.location.search`.
queryParam :: String -> Effect (Maybe String)
queryParam name = map toMaybe (_queryParam name)
