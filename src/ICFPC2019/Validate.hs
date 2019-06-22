module ICFPC2019.Validate where

import ICFPC2019.Utils
import ICFPC2019.Types
import ICFPC2019.StateUtils

--import Debug.Trace

validate :: Problem -> ProblemState -> [Action] -> Maybe (Int, Action)
--validate _ _ actions | trace ("validating! " ++ show actions) False = undefined
validate prob state actions = val state actions 0
  where
    val :: ProblemState -> [Action] -> Int -> Maybe (Int, Action)
--    val state (act:others) curIdx | trace ("val " ++ show (robotPosition $ problemRobot state) ++ " -> " ++ show curIdx ++ " " ++ show act) False = undefined
    val state []           curIdx = Nothing
    val state (act:others) curIdx =
      case changeState prob state act of
        Just state' -> val state' others (curIdx + 1)
        Nothing     -> Just (curIdx, act)
