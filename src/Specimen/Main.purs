module Specimen.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Specimen.Source (fetchText)
import Specimen.Preprocess (glyphify)
import Specimen.Block (extractBlocks)
import Specimen.Render (renderDocument)
import Specimen.Mount (setHtml)

main :: Effect Unit
main = launchAff_ do
  src <- fetchText "examples/Control.Monad.purs"
  liftEffect do
    log "[Specimen] source loaded"
    let blocks = extractBlocks (glyphify src)
    let html = renderDocument
                 { moduleSlug:  "Control.Monad"
                 , source:      "purescript-prelude 6.0.2"
                 , blocks
                 }
    setHtml "#specimen" html
    log "[Specimen] rendered"
