module ICFPC2019.Solver.Utils where

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Array.Repa as R
import Data.Maybe
import Linear.V2

import DNIWEChan.Graph
import ICFPC2019.Utils
import ICFPC2019.Types
import ICFPC2019.RobotUtils

import Debug.Trace

move' :: I2 -> Action -> I2
move' pos action = move pos action 1 

moveGetNeighbours :: Problem -> ProblemState -> [(ProblemState, Action)]
moveGetNeighbours problem@(Problem {..}) state@(ProblemState {..}) =
    [ ( state
        { problemUnwrapped = newUnwrapped
        , problemRobot = problemRobot { robotPosition = newPos }
        }
      , mov
      )
    | mov <- [MUp, MRight, MDown, MLeft]
    , let curPos = robotPosition problemRobot
    , let newPos = move' curPos mov
    , let newWrapped = map (+ newPos) $ S.toList $ robotManipulators problemRobot
    , let newUnwrapped = foldr S.delete problemUnwrapped newWrapped
    , checkBoundaries problemMap newPos
    , checkObstacles problemMap newPos
    ]

getNeighbours :: Problem -> ProblemState -> [(ProblemState, [Action], Int)]
getNeighbours problem@(Problem {..}) state
  | null usefulSteps = moveoutSteps
  | otherwise = take 1 usefulSteps
  where usefulSteps =
          [ ( state
              { problemUnwrapped = newUnwrapped
              , problemRobot = (problemRobot state) { robotPosition = newPos }
              }
            , [mov]
            , 1
            )
          | mov <- [MUp, MRight, MDown, MLeft]
          , let curPos = robotPosition $ problemRobot state
          , let newPos = move' curPos mov
          , let newWrapped = map (+ newPos) $ S.toList $ robotManipulators $ problemRobot state
          , let newUnwrapped = foldr S.delete (problemUnwrapped state) newWrapped
          , checkBoundaries problemMap newPos
          , checkObstacles problemMap newPos
          , S.size newUnwrapped /= S.size (problemUnwrapped state)
          ]

        moveoutSteps = map convertSteps $ maybeToList $ bfs (moveGetNeighbours problem) state hasMovedOut

        hasMovedOut state' = S.size (problemUnwrapped state') /= S.size (problemUnwrapped state)
        convertSteps steps = (finalState, actions, cost)
          where actions = map snd steps
                cost = length steps
                (finalState, _) = last steps

getAllNeighbours :: Problem -> ProblemState -> [(ProblemState, Action, Int)]
getAllNeighbours problem@(Problem {..}) state@(ProblemState {..}) = 
  let robot = problemRobot
      map_ = problemMap
      moves = [
          MUp, MRight, MDown, MLeft,
          MTurnRight, MTurnLeft,
          MAttachWheels, MAttachDrill, MPlaceBeacon
        ] ++ (MTeleport <$> (S.toList $ robotBeacons robot))
          ++ (MAttachManipulator <$> (S.toList $ manipulatorExtensionLocations $ robotManipulators robot))
      costs = [1..]
      newRobots = (applyAction robot map_ state) <$> moves
      validRobots = catMaybes $ map (\(mr, move, cost) -> case mr of
                                            Just r -> Just (r, move, cost)
                                            Nothing -> Nothing
                                    ) $ zip3 newRobots moves costs
      validManips r = validManipulators map_ (robotPosition r) (robotManipulators r)
      newWrapped r = map (+ (robotPosition r)) $ S.toList $ validManips r
      newState r move cost = (
          state {
            problemRobot = r,
            problemUnwrapped = foldr S.delete problemUnwrapped $ newWrapped r
          }, move, cost
        )
  in map (\(r, m, c) -> newState r m c) validRobots

genFinish :: ProblemState -> ProblemState
genFinish start@(ProblemState {..}) = start {problemUnwrapped = S.empty}

diffWrapped :: ProblemState -> ProblemState -> Int
diffWrapped startState endState = S.size $ startUnWrapped S.\\ endUnWrapped
  where
    startUnWrapped = problemUnwrapped startState
    endUnWrapped = problemUnwrapped endState

--diffTurn :: Int -> ProblemState -> ProblemState -> Int
--diffTurn mult startState endState = mult * (problemTurn endState - problemTurn startState)

addHeuristics :: (ProblemState -> ProblemState -> Int) -> (ProblemState -> ProblemState -> Int) -> ProblemState -> ProblemState -> Int
addHeuristics heur1 heur2 start end = heur1 start end + heur2 start end

--metric :: ProblemState -> ProblemState -> Int
--metric start end = tu + wr
--  where
--    wr = diffWrapped start end
--    tu = diffTurn 1 start end
