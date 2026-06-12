module SignalBoxV1 where

-- v1: name your atoms. Closed vocabularies; absence is Maybe, not "".
data Aspect = Red | Green

data PointsPos = ToLoop | ToMain

data SegmentId = SegA | SegL | SegM | SegE

type State =
  { sa :: Aspect, sl :: Aspect, se :: Aspect, sm :: Aspect
  , p1 :: PointsPos, p2 :: PointsPos
  , t1 :: Maybe SegmentId, t2 :: Maybe SegmentId
  }
