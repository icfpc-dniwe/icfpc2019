module ICFPC2019.Solver.AStar where

import qualified Data.Set as S
  
import DNIWEChan.Graph
import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.Solver.Utils

solve :: Problem -> ProblemState -> Maybe [(ProblemState, Action)]
solve problem state = aStar (getNeighbours problem) (S.size . problemUnwrapped) state (S.null . problemUnwrapped)
