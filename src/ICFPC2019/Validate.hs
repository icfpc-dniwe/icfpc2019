module ICFPC2019.Validate where

import ICFPC2019.Utils
import ICFPC2019.Types
import ICFPC2019.StateUtils


validate :: Problem -> ProblemState -> [Action] -> Maybe (Int, Action)
validate prob state trace = val prob state trace 0
  where
    val prob state [] curIdx = Nothing
    val prob state (act:others) curIdx =
      case changeState prob state act of
        Just state' -> val prob state' others (curIdx + 1)
        Nothing     -> Just (curIdx, act)
