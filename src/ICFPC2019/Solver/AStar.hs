module ICFPC2019.Solver.AStar where

import DNIWEChan.Graph
import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.Solver.Utils

solve :: Problem -> ProblemState -> Maybe [(ProblemState, Action)]
solve problem state = aStar (getNeighbours problem) diffWrapped state $ genFinish state
