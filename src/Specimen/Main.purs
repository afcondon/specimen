-- | The standalone viewer: mounts `Specimen.Component` into the page's
-- | `#specimen` element, typesetting whichever module the `?module=`
-- | query parameter names.
module Specimen.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (trim) as String
import Data.String.CodeUnits (dropRight, length, takeRight) as SCU
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

import Specimen.Component as Component
import Specimen.Source (queryParam)

main :: Effect Unit
main = HA.runHalogenAff do
  container <- HA.selectElement (QuerySelector mountPoint)
  case container of
    Nothing -> liftEffect (log ("[Specimen] nothing matches " <> mountPoint <> "; not mounting"))
    Just el -> do
      picked <- liftEffect (queryParam "module")
      let name = chooseModule picked
      liftEffect (log ("[Specimen] typesetting " <> name))
      void (runUI Component.component { moduleName: name, sourceLabel: sourceFor name } el)

mountPoint :: String
mountPoint = "#specimen"

chooseModule :: Maybe String -> String
chooseModule = case _ of
  Just raw | name <- normalizeModuleName raw, name /= "" -> name
  _ -> defaultModule

defaultModule :: String
defaultModule = "Control.Monad"

-- | Trim whitespace and any trailing dots. Defensive against terminals
-- | that over-greedily include a trailing period when extracting URLs
-- | from text.
normalizeModuleName :: String -> String
normalizeModuleName s =
  let t = String.trim s
  in if SCU.length t > 0 && SCU.takeRight 1 t == "."
       then normalizeModuleName (SCU.dropRight 1 t)
       else t

-- | Provenance for the colophon. The bundled examples are vendored from
-- | published packages at known versions; anything else a reader loads
-- | is their own.
sourceFor :: String -> String
sourceFor = case _ of
  "Control.Monad" -> "purescript-prelude 6.0.2"
  "Data.Variant" -> "purescript-variant 8.0.0"
  "Data.Profunctor" -> "purescript-profunctor 6.0.1"
  "Data.Profunctor.Strong" -> "purescript-profunctor 6.0.1"
  "Yoga.Om" -> "purescript-yoga-om 2.0.0"
  _ -> "user-supplied"
