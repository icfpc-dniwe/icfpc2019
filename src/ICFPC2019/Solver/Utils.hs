module ICFPC2019.Solver.Utils where

import qualified Data.Set as S
import qualified Data.Array.Repa as R
import Linear.V2
import ICFPC2019.Utils
import ICFPC2019.Types

move :: I2 -> Action -> I2
move (V2 x y) MUp    = V2 x (y + 1)
move (V2 x y) MRight = V2 (x + 1) y
move (V2 x y) MDown  = V2 x (y - 1)
move (V2 x y) MLeft  = V2 (x - 1) y
move (V2 x y) _         = V2 x y

checkBoundaries :: MapArray a -> I2 -> Bool
checkBoundaries gameMap = R.inShapeRange (V2 0 0) (R.extent gameMap - 1)

checkObstacles :: MapArray Cell -> I2 -> Bool
checkObstacles gameMap pos = (gameMap R.! pos) /= Obstacle

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
    , let newPos = move curPos mov
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

diffTurn :: Int -> ProblemState -> ProblemState -> Int
diffTurn mult startState endState = mult * ((problemTurn endState) - (problemTurn startState))

addHeuristics :: (ProblemState -> ProblemState -> Int) -> (ProblemState -> ProblemState -> Int) -> ProblemState -> ProblemState -> Int
addHeuristics heur1 heur2 start end = heur1 start end + heur2 start end

metric :: ProblemState -> ProblemState -> Int
metric start end = tu
  where
    wr = diffWrapped start end
    tu = diffTurn 1 start end
