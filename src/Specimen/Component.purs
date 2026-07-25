-- | The specimen viewer as a Halogen component.
-- |
-- | Its whole job is to run the pipeline — fetch source, glyphify,
-- | classify into blocks, hand the blocks to `Specimen.Render` — and to
-- | have somewhere honest to put the two states the old
-- | `fetch`-then-`innerHTML` version had no room for: not loaded yet,
-- | and failed to load.
module Specimen.Component
  ( component
  , Input
  , State
  , Status(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (try)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (message)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP

import Specimen.Block (Block, extractBlocks)
import Specimen.Preprocess (glyphify)
import Specimen.Render (renderDocument)
import Specimen.Source (fetchText)

-- | Which module to typeset, and how to credit it in the colophon.
type Input =
  { moduleName :: String
  , sourceLabel :: String
  }

data Status
  = Loading
  | Failed String
  | Ready (Array Block)

type State =
  { moduleName :: String
  , sourceLabel :: String
  , status :: Status
  }

data Action = Initialize

component :: forall q o m. MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        }
    }

initialState :: Input -> State
initialState { moduleName, sourceLabel } =
  { moduleName, sourceLabel, status: Loading }

render :: forall m. State -> H.ComponentHTML Action () m
render { moduleName, sourceLabel, status } = case status of
  Loading -> notice "specimen-loading" ("Setting " <> moduleName <> "…")
  Failed err -> notice "specimen-failed" ("Could not load " <> moduleName <> " — " <> err)
  Ready blocks ->
    renderDocument
      { moduleSlug: moduleName
      , source: sourceLabel
      , blocks
      , notes: Map.empty
      -- the viewer has no sidecars to offer
      , foreignSidecar: Nothing
      }
  where
  notice kind msg =
    HH.div [ HP.classes (map ClassName [ "specimen-notice", kind ]) ] [ HH.text msg ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    moduleName <- H.gets _.moduleName
    result <- H.liftAff (try (fetchText (examplePath moduleName)))
    H.modify_ _
      { status = case result of
          Left err -> Failed (message err)
          Right src -> Ready (extractBlocks (glyphify src))
      }

examplePath :: String -> String
examplePath moduleName = "examples/" <> moduleName <> ".purs"
