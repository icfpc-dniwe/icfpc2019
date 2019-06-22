module ICFPC2019.Solver.Utils where

import qualified Data.Set as S
import qualified Data.Array.Repa as R
import Linear.V2
import ICFPC2019.Utils
import ICFPC2019.Types
import ICFPC2019.RobotUtils

move' :: I2 -> Action -> I2
move' pos action = move pos action 1 

getNeighbours :: Problem -> ProblemState -> [(ProblemState, Action)]
getNeighbours problem@(Problem {..}) state@(ProblemState {..}) =
  [ ( state
      { problemUnwrapped = foldr S.delete problemUnwrapped newWrapped
      , problemRobot = problemRobot { robotPosition = newPos }
      }
    , mov
    )
    | mov <- [MUp, MRight, MDown, MLeft]
    , let curPos = robotPosition problemRobot
    , let newPos = move' curPos mov
    , let newWrapped = map (+ newPos) $ S.toList $ robotManipulators problemRobot
    , checkBoundaries problemMap newPos
    , checkObstacles problemMap newPos
  ]

genFinish :: ProblemState -> ProblemState
genFinish start@(ProblemState {..}) = start {problemUnwrapped = S.empty}

diffWrapped :: ProblemState -> ProblemState -> Int
diffWrapped startState endState = S.size $ startUnWrapped S.\\ endUnWrapped
  where
    startUnWrapped = problemUnwrapped startState
    endUnWrapped = problemUnwrapped endState