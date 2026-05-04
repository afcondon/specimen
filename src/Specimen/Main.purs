module Specimen.Main where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (trim) as String
import Data.String.CodeUnits (dropRight, length, takeRight) as SCU
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Specimen.Source (fetchText, queryParam)
import Specimen.Preprocess (glyphify)
import Specimen.Block (extractBlocks)
import Specimen.Render (renderDocument)
import Specimen.Mount (setHtml)

-- | Trim whitespace and any trailing dots. Defensive against terminals that
-- | over-greedily include a trailing period when extracting URLs from text.
normalizeModuleName :: String -> String
normalizeModuleName s =
  let t = String.trim s
  in if SCU.length t > 0 && SCU.takeRight 1 t == "."
       then normalizeModuleName (SCU.dropRight 1 t)
       else t

main :: Effect Unit
main = launchAff_ do
  pickedModule <- liftEffect (queryParam "module")
  let
    moduleName = case map normalizeModuleName pickedModule of
      Just m | m /= "" -> m
      _                -> "Control.Monad"
    sourcePkg = case moduleName of
      "Control.Monad"          -> "purescript-prelude 6.0.2"
      "Data.Variant"           -> "purescript-variant 8.0.0"
      "Data.Profunctor"        -> "purescript-profunctor 6.0.1"
      "Data.Profunctor.Strong" -> "purescript-profunctor 6.0.1"
      "Yoga.Om"                -> "purescript-yoga-om 2.0.0"
      _                        -> "user-supplied"
  src <- fetchText ("examples/" <> moduleName <> ".purs")
  liftEffect do
    log ("[Specimen] loaded " <> moduleName)
    let blocks = extractBlocks (glyphify src)
    let html = renderDocument
                 { moduleSlug: moduleName
                 , source:     sourcePkg
                 , blocks
                 , notes:      Map.empty
                 }
    setHtml "#specimen" html
    log "[Specimen] rendered"
