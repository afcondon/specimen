-- | Tabs for the shelf index.
-- |
-- | Nineteen books is more page than anyone scrolls, so the five
-- | shelves become tabs — each wearing its own band colour, so the tab
-- | bar reads as a key to the covers below it.
-- |
-- | This is an *island*: the page is fully rendered before any of this
-- | runs, and Halogen mounts into one empty `div`. With scripting off
-- | the tab bar simply never appears and every shelf stays visible,
-- | which is the whole reason the sections are hidden from here rather
-- | than being left out of the generated HTML.
-- |
-- | The tab bar is `Halogen.Widgets.SegmentedControl`, used the
-- | way the library's contract intends: it owns nothing. `active` lives
-- | in this component's state, the widget raises `Selected` as a
-- | *request*, and `handleAction` is what decides to honour it — which
-- | is also the only place the page's own sections get touched.
module Specimen.Shelf.Main (main) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

import Halogen.Widgets.SegmentedControl as Segmented

import Specimen.Shelf.Page (Shelf, readShelves, showOnly)

mountPoint :: String
mountPoint = "#shelf-tabs"

main :: Effect Unit
main = HA.runHalogenAff do
  shelves <- liftEffect readShelves
  container <- HA.selectElement (QuerySelector mountPoint)
  case container, Array.head shelves of
    -- Nothing to mount into, or nothing to tab between: leave the page
    -- exactly as it was served.
    Just el, Just first -> void (runUI component { shelves, initial: first.key } el)
    _, _ -> pure unit

type Input =
  { shelves :: Array Shelf
  , initial :: String
  }

type State =
  { shelves :: Array Shelf
  , active :: String
  }

type Slots = (segmented :: Segmented.Slot Unit)

_segmented :: Proxy "segmented"
_segmented = Proxy

data Action
  = Initialize
  | Select String

component :: forall q o m. MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState: \{ shelves, initial } -> { shelves, active: initial }
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render { shelves, active } =
  HH.slot _segmented unit Segmented.component
    ((Segmented.defaultInput (map tab shelves)) { active = active })
    (\(Segmented.Selected key) -> Select key)
  where
  tab s = { key: s.key, label: s.label, color: Just s.color }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> H.gets _.active >>= (liftEffect <<< showOnly)
  Select key -> do
    H.modify_ _ { active = key }
    liftEffect (showOnly key)
