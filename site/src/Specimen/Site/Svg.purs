-- | The book's three drawn marks: the waxseal that closes the colophon
-- | and doubles as the favicon, the banner plate the shelf page shows,
-- | and the release sparkline.
-- |
-- | Built as Halogen trees and serialised with the same string renderer
-- | `Specimen.Render` uses, so the whole project has one way of turning
-- | a description of markup into markup.
-- |
-- | These are typeset drawings, not charts: the attributes that matter
-- | are `letter-spacing`, `paint-order`, `textPath`. `halogen-svg-elems`
-- | types the common geometric surface and would send most of this
-- | through an escape hatch anyway, so the elements are named directly
-- | and a small vocabulary at the foot of the module keeps the drawing
-- | code readable.
module Specimen.Site.Svg
  ( waxseal
  , sealTree
  , banner
  , sparkline
  , Release
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Number.Format (fixed, toStringWith)
import Data.String as String
import Data.String.Pattern (Pattern(..), Replacement(..))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.Prop (Prop(..))
import Halogen.VDom.StringRenderer as StringRenderer

import Specimen.Site.Layout (BookModule)
import Specimen.Site.Pack (SealCircle)

-- ----------------------------------------------------------------------------
-- Waxseal

sealOuter :: Number
sealOuter = 240.0

sealPack :: Number
sealPack = 190.0

-- | A B-ink seal: the package's namespace tree packed into a disc,
-- | ringed by its own name and signed with a formal mark.
waxseal :: { name :: String, mark :: String, seal :: Array SealCircle } -> String
waxseal = render <<< sealTree { opaque: true }

-- | The seal as a tree, so the colophon can place the same drawing the
-- | `.svg` file gets. On the page it sits on the paper already, so it
-- | wants no backing of its own; as a file and as a favicon it does.
sealTree
  :: forall w i
   . { opaque :: Boolean }
  -> { name :: String, mark :: String, seal :: Array SealCircle }
  -> HH.HTML w i
sealTree { opaque } { name, mark, seal } =
    svgRoot (2.0 * sealOuter) (2.0 * sealOuter)
      ( (if opaque
           then [ el "rect" [ attr "width" "100%", attr "height" "100%", attr "fill" "#fff" ] [] ]
           else [])
      <> [ ring (sealOuter - 6.0) 3.0
      , ring (sealOuter - 46.0) 1.2
      , el "defs" [] [ el "path" [ attr "id" "rim", attr "d" rimPath ] [] ]
      , el "text"
          [ attr "font-family" "Inter, sans-serif"
          , attr "font-size" "17"
          , attr "font-weight" "600"
          , attr "letter-spacing" "7"
          , attr "fill" "#111"
          ]
          [ el "textPath"
              [ attr "href" "#rim", attr "startOffset" "50%", attr "text-anchor" "middle" ]
              [ HH.text inscription ]
          ]
      , el "text"
          [ num "x" sealOuter
          , num "y" (2.0 * sealOuter - 22.0)
          , attr "font-family" "Georgia, serif"
          , attr "font-size" "30"
          , attr "text-anchor" "middle"
          , attr "fill" "#111"
          ]
          [ HH.text mark ]
      , el "g" [ attr "transform" "translate(0,6)" ] (map sealCircle seal)
      ])
  where
  offset = sealOuter - sealPack

  ring r width =
    el "circle"
      [ num "cx" sealOuter, num "cy" sealOuter, num "r" r
      , attr "fill" "none", attr "stroke" "#111", num "stroke-width" width
      ]
      []

  -- A semicircle for the inscription to run along.
  rimPath =
    let r = sealOuter - 25.0
    in "M " <> n (sealOuter - r) <> " " <> n sealOuter
         <> " a " <> n r <> " " <> n r <> " 0 1 1 " <> n (2.0 * r) <> " 0"

  inscription =
    "PURESCRIPT · " <> String.toUpper (String.replaceAll (Pattern "-") (Replacement " ") name)

  sealCircle c =
    el "circle"
      ([ num "cx" (c.x + offset), num "cy" (c.y + offset), num "r" c.r ]
        <> if c.container
             then [ attr "fill" "none", attr "stroke" "#111", num "stroke-width" 1.3 ]
             else [ attr "fill" "#111" ])
      []

-- ----------------------------------------------------------------------------
-- Banner

bannerWidth :: Number
bannerWidth = 1600.0

-- | The static banner plate: the import graph as a swarm of module
-- | discs, each packed with its own declarations. The book page animates
-- | this; the shelf shows it still.
banner :: Array BookModule -> String
banner mods =
  render $
    el "svg"
      [ attr "xmlns" "http://www.w3.org/2000/svg"
      , attr "viewBox" ("0 0 " <> n bannerWidth <> " " <> n height)
      ]
      (map edge pairs <> map plate mods)
  where
  height = if Array.length mods < 6 then 380.0 else 640.0
  top = 40.0
  labelSize = if Array.length mods >= 30 then 8.5 else 11.0

  at m = { x: m.ax * bannerWidth, y: top + m.ay * (height - 2.0 * top) }
  bySlug slug = Array.find (\m -> m.slug == slug) mods

  pairs = Array.concatMap
    (\m -> Array.mapMaybe (\s -> map { from: m, to: _ } (bySlug s)) m.imports)
    mods

  -- A flattened cubic through the midpoint, so edges read as a weave
  -- rather than a tangle.
  edge { from, to } =
    let a = at from
        b = at to
        mid = (a.x + b.x) / 2.0
    in el "path"
         [ attr "d" ("M " <> p1 a.x <> " " <> p1 a.y
             <> " C " <> p1 mid <> " " <> p1 a.y
             <> ", " <> p1 mid <> " " <> p1 b.y
             <> ", " <> p1 b.x <> " " <> p1 b.y)
         , attr "fill" "none", attr "stroke" "#111", num "stroke-width" 0.6
         , attr "opacity" "0.10"
         ]
         []

  plate m =
    let { x, y } = at m
    in el "g" [ attr "transform" ("translate(" <> p1 x <> "," <> p1 y <> ")") ]
         ( [ el "circle"
               [ attr "r" (p1 m.rA), attr "fill" "#fff"
               , attr "stroke" "#111", num "stroke-width" 1.5
               ] []
           ]
             <> map declaration m.plate
             <> [ el "text"
                    [ attr "y" (p1 (negate (m.rA + 10.0)))
                    , attr "text-anchor" "middle"
                    , attr "font-family" "Inter, sans-serif"
                    , num "font-size" labelSize
                    , attr "letter-spacing" "0.08em"
                    , attr "fill" "#777"
                    -- painting the stroke first haloes the label so it
                    -- stays legible where it crosses a disc
                    , attr "stroke" "rgba(250,250,247,0.88)"
                    , num "stroke-width" 3.0
                    , attr "paint-order" "stroke"
                    , attr "stroke-linejoin" "round"
                    ]
                    [ HH.text m.name ]
                ]
         )

  declaration d =
    el "circle" [ num "cx" d.dx, num "cy" d.dy, num "r" d.r, attr "fill" "#111" ] []

-- ----------------------------------------------------------------------------
-- Release sparkline

type Release =
  { version :: String
  , at :: Number -- epoch millis
  , major :: Boolean
  }

-- | A timeline of releases: left edge the first publish, right edge
-- | today. The empty stretch after the last tick is the point being
-- | made — in this ecosystem a library that hasn't changed in years is
-- | usually finished, not abandoned.
sparkline :: forall w i. { releases :: Array Release, now :: Number } -> HH.HTML w i
sparkline { releases, now } =
  el "svg"
    [ attr "class" "version-spark"
    , attr "viewBox" ("0 0 " <> n width <> " " <> n height)
    , num "width" width
    , num "height" height
    , attr "role" "img"
    , attr "aria-label" "release timeline"
    ]
    ( [ axis ] <> map tick releases <> [ head, startYear, endYear ] )
  where
  width = 300.0
  height = 32.0
  pad = 4.0
  axisY = 20.0

  first = fromMaybe now (map _.at (Array.head releases))
  x t = pad + (width - 2.0 * pad) * (if now == first then 0.0 else (t - first) / (now - first))

  axis =
    el "line"
      [ num "x1" pad, num "y1" axisY, num "x2" (width - pad), num "y2" axisY
      , attr "stroke" "#d8d6cf", num "stroke-width" 1.0
      ] []

  tick r =
    el "line"
      [ attr "x1" (p1 (x r.at)), num "y1" (axisY - (if r.major then 13.0 else 8.0))
      , attr "x2" (p1 (x r.at)), num "y2" axisY
      , attr "stroke" "#1a1a1a", num "stroke-width" (if r.major then 1.6 else 1.0)
      ] []

  -- an open circle at today's edge: the timeline is still running
  head =
    el "circle"
      [ num "cx" (width - pad), num "cy" axisY, num "r" 2.5
      , attr "fill" "none", attr "stroke" "#1a1a1a", num "stroke-width" 1.0
      ] []

  startYear = yearLabel pad "start" (year first)
  endYear =
    yearLabel (width - pad) "end"
      (if year now == year first then "today" else year now)

  yearLabel px anchor text =
    el "text"
      ([ num "x" px, num "y" (height - 1.0)
       , attr "font-family" "Inter, sans-serif", attr "font-size" "8"
       , attr "letter-spacing" "0.1em", attr "fill" "#8a877e"
       ] <> if anchor == "end" then [ attr "text-anchor" "end" ] else [])
      [ HH.text text ]

foreign import year :: Number -> String

-- ----------------------------------------------------------------------------
-- Vocabulary

el :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
el name = HH.element (HH.ElemName name)

attr :: forall r i. String -> String -> HP.IProp r i
attr name = HP.attr (HH.AttrName name)

-- | An attribute whose value is a number. `show` on a `Number` gives
-- | `1.0` where SVG wants `1`, so whole values lose the tail.
num :: forall r i. String -> Number -> HP.IProp r i
num name = attr name <<< n

-- | `show` on a `Number` gives `480.0` where SVG wants `480`; whole
-- | values lose the tail.
n :: Number -> String
n value = fromMaybe shown (String.stripSuffix (Pattern ".0") shown)
  where
  shown = show value

-- | One decimal place, matching what the generator has always written.
p1 :: Number -> String
p1 = toStringWith (fixed 1)

svgRoot :: forall w i. Number -> Number -> Array (HH.HTML w i) -> HH.HTML w i
svgRoot w h =
  el "svg"
    [ attr "xmlns" "http://www.w3.org/2000/svg"
    , attr "viewBox" ("0 0 " <> n w <> " " <> n h)
    , num "width" w
    , num "height" h
    ]

-- | Serialise as XML rather than as HTML.
-- |
-- | `Halogen.VDom.DOM.StringRenderer` knows HTML's void elements, so it
-- | writes `<circle></circle>` — well-formed, but it near enough doubles
-- | a banner with a thousand circles in it. In XML any childless element
-- | may close itself, which is what `SelfClosingTag` for every tag
-- | means: the renderer only takes it up when there are no children.
-- |
-- | Attributes are all we emit — the vocabulary below never reaches for
-- | a property or a handler — so rendering them is a one-liner rather
-- | than a reimplementation of the DOM renderer's coercion cases.
render :: HH.PlainHTML -> String
render = StringRenderer.render (const StringRenderer.SelfClosingTag) renderAttrs absurd <<< unwrap

renderAttrs :: forall i. Array (Prop i) -> String
renderAttrs = String.joinWith " " <<< Array.mapMaybe case _ of
  Attribute _ name value -> Just (name <> "=\"" <> escapeAttr value <> "\"")
  _ -> Nothing

-- | XML attribute escaping: the four characters that actually have to
-- | be escaped, and no others. The vdom's own escaper is written for
-- | HTML and also encodes `/`, which would leave the namespace
-- | declaration reading `http:&#x2F;&#x2F;` — legal, since attribute
-- | values are normalised before namespaces are resolved, but not
-- | something to make a document depend on.
escapeAttr :: String -> String
escapeAttr =
  replace "&" "&amp;"
    >>> replace "<" "&lt;"
    >>> replace ">" "&gt;"
    >>> replace "\"" "&quot;"
  where
  replace from to = String.replaceAll (Pattern from) (Replacement to)
