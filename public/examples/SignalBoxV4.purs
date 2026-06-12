module SignalBoxV4 (State, make) where

-- v4: parse, don't validate. The remaining failure families tie routes
-- to occupancy. So the type is opaque and the only door is total.
newtype State = State
  { locked :: Locked
  , p1 :: PointsPos, p2 :: PointsPos
  , t1 :: Maybe SegmentId, t2 :: Maybe SegmentId
  }

make :: Locked -> PointsPos -> PointsPos -> Maybe SegmentId -> Maybe SegmentId -> Either (Array Violation) State
make locked p1 p2 t1 t2 = validate (State { locked, p1, p2, t1, t2 })
