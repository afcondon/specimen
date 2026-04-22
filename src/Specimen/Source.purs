module Specimen.Source
  ( fetchText
  ) where

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import _fetchText :: String -> Effect (Promise String)

-- | Fetch a URL as plain text. Throws via the underlying Aff if the request fails.
fetchText :: String -> Aff String
fetchText url = toAffE (_fetchText url)
