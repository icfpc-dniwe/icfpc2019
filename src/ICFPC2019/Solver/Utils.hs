module ICFPC2019.Solver.Utils where

import qualified Data.Set as S
import Data.Array.Repa
import Linear.V2
import ICFPC2019.Utils
import ICFPC2019.Types

move :: I2 -> Action -> I2
move (V2 x y) MUp    = V2 x (y + 1)
move (V2 x y) MRight = V2 (x + 1) y
move (V2 x y) MDown  = V2 x (y - 1)
move (V2 x y) MLeft  = V2 (x - 1) y
move (V2 x y) _         = V2 x y

checkBoundaries :: MapArr a -> I2 -> Bool
checkBoundaries gameMap = inShapeRange (V2 0 0) (extent gameMap - 1)

checkObstacles :: MapArr Cell -> I2 -> Bool
checkObstacles gameMap pos = (gameMap ! pos) /= Obstacle

wrapPos :: S.Set I2 -> MapArr Cell -> MapArr Cell
wrapPos positions curMap = computeS $ fromFunction (extent curMap) wrap
  where
    wrap pos = if S.member pos positions
               then if curMap ! pos == Obstacle
                    then Obstacle
                    else Free {cellWrapped = True, cellObjects = S.empty}
               else curMap ! pos

getNeighbours :: Problem -> [(Problem, Action)]
getNeighbours Problem {..} =
  [ ( Problem
       { problemMap = wrapPos toWrap problemMap
       , problemRobot = Robot {robotPosition = newPos, robotManipulators = manipulators}
       , problemOffset = problemOffset
       }
    , mov
    )
    | mov <- [MUp, MRight, MDown, MLeft]
    , let curPos = robotPosition problemRobot
    , let manipulators = robotManipulators problemRobot
    , let newPos = move curPos mov
    , let toWrap = S.map (\pos -> newPos + pos) manipulators
    , checkBoundaries problemMap newPos
    , checkObstacles problemMap newPos
  ]
