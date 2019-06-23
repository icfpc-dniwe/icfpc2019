module ICFPC2019.Solver.BFS where

--import Data.Sequence (Sequence)
--import qualified Data.Sequence as Q
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import qualified Data.Set as DS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)

import DNIWEChan.Graph
import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.Solver.Utils

solve :: Problem -> ProblemState -> Maybe [(ProblemState, [Action])]
solve problem state = bfs (map (\(st, tag, _) -> (st, tag)) . getNeighbours defaultPriorities problem 0) state checker
  where
    checker curState = DS.null $ problemUnwrapped curState
