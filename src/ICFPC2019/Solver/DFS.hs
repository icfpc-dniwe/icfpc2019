module ICFPC2019.Solver.DFS where

import qualified Data.Set as DS
import DNIWEChan.Graph
import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.Solver.Utils

solve :: Problem -> ProblemState -> Maybe [(ProblemState, [Action])]
solve problem state = dfs (map (\(st, tag, _) -> (st, tag)) . getNeighbours defaultPriorities problem) state checker
  where
    checker curState = DS.null $ problemUnwrapped curState
