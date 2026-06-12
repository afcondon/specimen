module SignalBoxV3 where

-- v3: the locking table as a type. Legal combinations of routes are a
-- closed sum: the constructor for "conflicting greens" does not exist.
data Locked
  = NoneSet
  | One Route
  | PassEntry   -- A->L together with E->M: two trains enter to pass
  | PassExit    -- L->E together with M->A: two trains depart after passing

type State =
  { locked :: Locked
  , p1 :: PointsPos, p2 :: PointsPos
  , t1 :: Maybe SegmentId, t2 :: Maybe SegmentId
  }
