-- | A deterministic force settle, computed once at build time.
-- |
-- | The book needs two arrangements of the same discs: a wide banner
-- | across the top of the page, and a narrow rail down the left margin.
-- | Both pin each module to its import layer and let collision push
-- | siblings apart along the free axis; the page interpolates between
-- | the two as the reader scrolls.
-- |
-- | **Why this is hand-written.** Hylograph's force engine
-- | (`hylograph-simulation`) is an FFI wrapper over `d3-force`: adopting
-- | it here would have renamed the dependency, not removed it. It is
-- | also built for animating in a browser — it ticks against a DOM
-- | container and emits events — whereas these positions are a build
-- | artifact, baked into `banner.svg` and the page's payload, and have
-- | to be identical on every run.
-- |
-- | So this is the three forces the layout actually uses, and nothing
-- | else: `positionX`, `positionY`, `collide`. It follows d3's
-- | velocity-Verlet scheme — same alpha schedule, same velocity decay,
-- | same in-place collision resolution — and on a handful of discs it
-- | reproduces d3 to the last bit. There is no quadtree: with a
-- | package's worth of modules the pairwise loop is not the slow part
-- | of a build, and leaving it out keeps the whole thing readable.
-- |
-- | On a crowded book the arrangement *does* drift from d3's, and the
-- | reason is worth knowing. Every module on an import layer is pulled
-- | to an identical target, so discs land on exactly the same
-- | coordinate constantly; d3 breaks those ties with a jitter drawn
-- | from a seeded generator, consumed in quadtree-traversal order. Six
-- | hundred ticks amplify that infinitesimal noise into visibly
-- | different — not better or worse — positions. Matching it would mean
-- | porting d3's quadtree purely to consume randomness in the same
-- | sequence. `perturb` below breaks the ties instead.
-- |
-- | This wants to live in `hylograph-layout` eventually — a pure settle
-- | has no business being package-specific — but it is small enough to
-- | prove out here first.
module Specimen.Site.Beeswarm
  ( Node
  , Config
  , Placed
  , settle
  , defaultConfig
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal (for) as ST
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Maybe (fromMaybe)
import Data.Number (pow, remainder, sqrt)

-- | One disc entering the simulation. `targetX`/`targetY` are what the
-- | positioning forces pull towards; `radius` is what collision keeps
-- | clear.
type Node =
  { x :: Number
  , y :: Number
  , targetX :: Number
  , targetY :: Number
  , radius :: Number
  }

type Placed =
  { x :: Number
  , y :: Number
  }

type Config =
  { ticks :: Int
  , strengthX :: Number
  , strengthY :: Number
  , collideIterations :: Int
  , collideStrength :: Number
  , velocityDecay :: Number
  , alphaDecay :: Number
  , alphaTarget :: Number
  }

-- | d3's defaults: alpha decays from 1 towards `alphaTarget` at a rate
-- | that reaches alphaMin (0.001) in 300 ticks, and each node keeps 60%
-- | of its velocity per tick. (d3 stores that 0.6 internally while its
-- | `velocityDecay` accessor reports the complement, 0.4 — the number
-- | that matters here is the multiplier.)
defaultConfig :: Config
defaultConfig =
  { ticks: 600
  , strengthX: 0.1
  , strengthY: 0.1
  , collideIterations: 1
  , collideStrength: 1.0
  , velocityDecay: 0.6
  , alphaDecay: 1.0 - pow 0.001 (1.0 / 300.0)
  , alphaTarget: 0.0
  }

-- | Run the simulation to a standstill and hand back final positions,
-- | in input order.
settle :: Config -> Array Node -> Array Placed
settle config nodes = ST.run do
  xs <- STArray.thaw (map _.x nodes)
  ys <- STArray.thaw (map _.y nodes)
  vxs <- STArray.thaw (map (const 0.0) nodes)
  vys <- STArray.thaw (map (const 0.0) nodes)
  alphaRef <- STRef.new 1.0
  seedRef <- STRef.new 1.0

  let
    count = Array.length nodes
    targetXs = map _.targetX nodes
    targetYs = map _.targetY nodes
    radii = map _.radius nodes

    read arr i = fromMaybe 0.0 <$> STArray.peek i arr
    bump arr i d = void (STArray.modify i (_ + d) arr)

    -- vx += (target - x) * strength * alpha
    position arr varr targets strength alpha =
      ST.for 0 count \i -> do
        here <- read arr i
        bump varr i ((at targets i - here) * strength * alpha)

    -- Resolve overlaps by nudging both discs apart along the line
    -- between them, split in proportion to area. Velocities are read
    -- and written in place within a pass, so later pairs see earlier
    -- corrections.
    --
    -- Two discs can sit at exactly the same coordinate on one axis —
    -- it happens constantly here, because every module on an import
    -- layer is pulled to the identical target. The separation vector is
    -- then zero on that axis and the pair would stay stacked forever,
    -- so the tie is broken with an imperceptible nudge from `perturb`.
    collide =
      ST.for 0 config.collideIterations \_ ->
        ST.for 0 count \i -> do
          xi0 <- read xs i
          yi0 <- read ys i
          vxi <- read vxs i
          vyi <- read vys i
          let
            xi = xi0 + vxi
            yi = yi0 + vyi
            ri = at radii i
          ST.for 0 count \j ->
            when (j > i) do
              xj0 <- read xs j
              yj0 <- read ys j
              vxj <- read vxs j
              vyj <- read vys j
              let
                rj = at radii j
                r = ri + rj
                dx = xi - (xj0 + vxj)
                dy = yi - (yj0 + vyj)
              when (dx * dx + dy * dy < r * r) do
                dx' <- if dx == 0.0 then perturb else pure dx
                dy' <- if dy == 0.0 then perturb else pure dy
                let l2 = dx' * dx' + dy' * dy'
                when (l2 > 0.0) do
                  let
                    l = sqrt l2
                    scale = (r - l) / l * config.collideStrength
                    nx = dx' * scale
                    ny = dy' * scale
                    -- share the correction by area, so a big disc moves less
                    share = (rj * rj) / (ri * ri + rj * rj)
                  bump vxs i (nx * share)
                  bump vys i (ny * share)
                  bump vxs j (negate (nx * (1.0 - share)))
                  bump vys j (negate (ny * (1.0 - share)))

    -- A tiny signed offset, from a seeded linear congruential generator.
    -- It exists only to break exact ties, and is five orders of
    -- magnitude below a pixel — but the simulation is chaotic enough
    -- that the *sequence* still decides the final arrangement, which is
    -- why the seed is fixed here rather than drawn from the clock. Same
    -- input, same book, on every machine.
    perturb = do
      s <- STRef.read seedRef
      let next = remainder (1664525.0 * s + 1013904223.0) 4294967296.0
      void (STRef.write next seedRef)
      pure ((next / 4294967296.0 - 0.5) * 1.0e-6)

    -- x += vx *= velocityDecay
    integrate =
      ST.for 0 count \i -> do
        vx <- read vxs i
        vy <- read vys i
        let
          vx' = vx * config.velocityDecay
          vy' = vy * config.velocityDecay
        void (STArray.poke i vx' vxs)
        void (STArray.poke i vy' vys)
        bump xs i vx'
        bump ys i vy'

  ST.for 0 config.ticks \_ -> do
    alpha0 <- STRef.read alphaRef
    let alpha = alpha0 + (config.alphaTarget - alpha0) * config.alphaDecay
    void (STRef.write alpha alphaRef)
    position xs vxs targetXs config.strengthX alpha
    position ys vys targetYs config.strengthY alpha
    collide
    integrate

  finalX <- STArray.freeze xs
  finalY <- STArray.freeze ys
  pure (Array.zipWith { x: _, y: _ } finalX finalY)

  where
  at arr i = fromMaybe 0.0 (Array.index arr i)
