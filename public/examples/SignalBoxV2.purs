module SignalBoxV2 where

-- v2: derive, don't store. Signals are outputs of the lever frame,
-- not lamps somebody sets. A derived aspect can't contradict the points.
type State =
  { wantAL :: Boolean, wantLE :: Boolean, wantEM :: Boolean, wantMA :: Boolean
  , p1 :: PointsPos, p2 :: PointsPos
  , t1 :: Maybe SegmentId, t2 :: Maybe SegmentId
  }

greens :: State -> Array Route
greens s = filter (\r -> leverFor r s == requiredBy r) (requested s)
