module SignalBoxV0 where

-- v0: the state, as too many codebases keep it.
type State =
  { sa :: String, sl :: String, se :: String, sm :: String   -- "G"? "green"? "g"? ""?
  , p1 :: Boolean, p2 :: Boolean    -- true means... toward the loop? (check the wiki)
  , t1 :: String, t2 :: String      -- segment name, "" when off-scene, typos welcome
  }
