-- | The book page.
-- |
-- | One typed tree from the masthead to the colophon: the articles come
-- | back from `Specimen.Render` as Halogen HTML and are placed here
-- | rather than spliced in as text, and the seal is the same drawing the
-- | `.svg` file gets. Nothing on this page is assembled by string
-- | concatenation, which is why the FFI markers below can be an
-- | attribute on a node instead of a search-and-replace over markup.
module Specimen.Site.Book
  ( Facts
  , Page
  , page
  , article
  , Html
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer as StringRenderer

import Specimen.Site.Svg (Release, sparkline)

-- | The page is built with no actions in it — it is a document, not a
-- | component — so both slots are uninhabited.
type Html = HH.HTML Void Void

type Facts =
  { title :: String
  , deck :: String
  , sourceLabel :: String
  , modules :: Int
  , declarations :: Int
  , lines :: Int
  , stableSince :: Maybe String
  , today :: String
  }

type Page =
  { facts :: Facts
  , articles :: Array Html
  , seal :: Html
  , favicon :: String
  , releases :: Array Release
  , now :: Number
  , payload :: String
  }

-- | Serialise the whole page.
page :: Page -> String
page p =
  "<!DOCTYPE html>\n" <> StringRenderer.render absurd (unwrap (document p))

document :: Page -> HH.HTML Void Void
document p =
  HH.html [ HP.attr (HH.AttrName "lang") "en" ]
    [ HH.head_ (headOf p)
    , HH.body_ (bodyOf p)
    ]

headOf :: Page -> Array (HH.HTML Void Void)
headOf { facts, favicon } =
  [ HH.meta [ HP.charset "UTF-8" ]
  , HH.meta
      [ HP.name "viewport", HP.attr (HH.AttrName "content") "width=device-width, initial-scale=1.0" ]
  , HH.title_ [ HH.text (facts.title <> " — a specimen book") ]
  , HH.link [ HP.rel "icon", HP.href favicon ]
  , HH.link [ HP.rel "preconnect", HP.href "https://fonts.googleapis.com" ]
  , HH.link [ HP.rel "stylesheet", HP.href googleFonts ]
  ] <> map stylesheet [ "sigil.css", "style.css", "book.css" ]
  where
  stylesheet name = HH.link [ HP.rel "stylesheet", HP.href name ]

googleFonts :: String
googleFonts =
  "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500;600&display=swap"

bodyOf :: Page -> Array (HH.HTML Void Void)
bodyOf p =
  [ masthead p
  -- The stage the swarm is drawn into; book.js owns everything in it.
  , HH.element (HH.ElemName "svg") [ HP.id "stage" ] []
  , HH.main [ HP.id "book" ]
      [ HH.div [ cls "book-body" ] p.articles
      , colophon p
      ]
  , ffiModal
  , HH.element (HH.ElemName "script")
      [ HP.attr (HH.AttrName "type") "application/json", HP.id "book-index" ]
      [ HH.text p.payload ]
  , HH.element (HH.ElemName "script")
      [ HP.attr (HH.AttrName "type") "module", HP.src "book.js" ]
      []
  ]

masthead :: Page -> HH.HTML Void Void
masthead { facts, releases, now } =
  HH.header [ HP.id "hero-title", cls "book-head" ]
    ( [ HH.div [ cls "book-kicker" ] [ HH.text "PureScript · specimen book" ]
      , HH.h1 [ cls "book-title" ] [ HH.text facts.title ]
      , HH.p [ cls "book-deck" ] [ HH.text facts.deck ]
      ]
        <> versions
    )
  where
  versions
    | Array.null releases = []
    | otherwise =
        [ HH.div [ cls "book-versions" ]
            [ sparkline { releases, now }
            , HH.p_ [ HH.text (releaseSummary facts.stableSince releases) ]
            ]
        ]

releaseSummary :: Maybe String -> Array Release -> String
releaseSummary stableSince releases = case Array.length releases, stableSince of
  1, Just since -> "1 release · published " <> since
  n, Just since -> show n <> " releases · stable since " <> since
  n, Nothing -> show n <> " releases"

colophon :: Page -> HH.HTML Void Void
colophon { facts, seal } =
  HH.footer [ cls "book-colophon" ]
    [ seal
    , HH.div [ cls "facts" ]
        [ HH.text (facts.sourceLabel <> stability)
        , HH.br_
        , HH.text (show facts.modules <> " modules · " <> show facts.declarations
            <> " declarations · " <> show facts.lines <> " lines")
        , HH.br_
        , HH.text ("typeset by specimen · " <> facts.today)
        ]
    ]
  where
  stability = case facts.stableSince of
    Just since -> " · stable since " <> since
    Nothing -> ""

-- | Empty chrome for the foreign-implementation overlay; `book.js` fills
-- | it in when a foreign row is clicked.
ffiModal :: HH.HTML Void Void
ffiModal =
  HH.div [ HP.id "ffi-modal", HP.attr (HH.AttrName "hidden") "" ]
    [ HH.div [ cls "ffi-backdrop" ] []
    , HH.div [ cls "ffi-card" ]
        [ HH.header_
            [ HH.span [ cls "ffi-title" ] []
            , HH.span [ cls "ffi-sub" ] [ HH.text "foreign implementation" ]
            , HH.nav_ []
            , HH.button
                [ cls "ffi-close", HP.attr (HH.AttrName "aria-label") "close" ]
                [ HH.text "✕" ]
            ]
        , HH.pre_ []
        ]
    ]

-- | One module's article, carrying the id the scroll-spy pairs with its
-- | bubble and the accent its shelf colour comes from.
article
  :: { slug :: String, name :: String, accent :: String }
  -> Html
  -> Html
article { slug, name, accent } body =
  HH.div
    [ cls "book-module"
    , HP.id ("mod-" <> slug)
    , HP.attr (HH.AttrName "data-module") name
    , HP.attr (HH.AttrName "style") ("--accent: " <> accent)
    ]
    [ body ]

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< ClassName
