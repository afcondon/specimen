module Specimen.Mount
  ( setHtml
  ) where

import Prelude
import Effect (Effect)

foreign import setHtml :: String -> String -> Effect Unit
