module Specimen.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Specimen.Source (fetchText, queryParam)
import Specimen.Preprocess (glyphify)
import Specimen.Block (extractBlocks)
import Specimen.Render (renderDocument)
import Specimen.Mount (setHtml)

main :: Effect Unit
main = launchAff_ do
  pickedModule <- liftEffect (queryParam "module")
  let
    moduleName = case pickedModule of
      Just m | m /= "" -> m
      _                -> "Control.Monad"
    sourcePkg = case moduleName of
      "Control.Monad" -> "purescript-prelude 6.0.2"
      "Data.Variant"  -> "purescript-variant 8.0.0"
      "Yoga.Om"       -> "purescript-yoga-om 2.0.0"
      _               -> "user-supplied"
  src <- fetchText ("examples/" <> moduleName <> ".purs")
  liftEffect do
    log ("[Specimen] loaded " <> moduleName)
    let blocks = extractBlocks (glyphify src)
    let html = renderDocument
                 { moduleSlug: moduleName
                 , source:     sourcePkg
                 , blocks
                 }
    setHtml "#specimen" html
    log "[Specimen] rendered"
