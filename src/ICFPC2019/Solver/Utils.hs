module ICFPC2019.Solver.Utils where

import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Set as S
import qualified Data.Array.Repa as R
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Linear.V2

import DNIWEChan.Graph
import ICFPC2019.Utils
import ICFPC2019.Types
import ICFPC2019.RobotUtils

import Debug.Trace

collectBooster :: Problem -> ProblemState -> Booster -> ProblemState
collectBooster problem@(Problem {..}) state@(ProblemState {..}) bst =
  let robot = problemRobot
      map_ = problemMap
      executeAction action =
        let actionResult = applyAction robot map_ state action
        in
          case actionResult of
            Just r -> state { problemRobot = r }
            Nothing -> state
  in
    case bst of
      Extension -> executeAction MPickUpManipulator
      FastWheels -> executeAction MPickUpWheels
      Drill -> executeAction MPickUpDrill
      Teleport -> executeAction MPickUpBeacon
      Mysterious -> state

collectBoosters' :: Problem  -> [Booster] -> ProblemState -> ProblemState
collectBoosters' _ [] s = s
collectBoosters' problem (h:t) s = collectBoosters' problem t $ collectBooster problem s h

collectBoosters :: Problem -> [I2] -> ProblemState -> ProblemState
collectBoosters _ [] state = state
collectBoosters problem@(Problem {..}) (h:t) state@(ProblemState {..}) = collectBoosters problem t $
  let robot = problemRobot
      pos = h
      collectAt pos st = collectBoosters' problem (S.toList $ problemBoosters M.! pos) st
      removeAt pos st = st { problemBoosters = M.delete pos problemBoosters }
  in
    if (M.member pos problemBoosters)
      then removeAt pos $ collectAt pos state
      else state

getAllMoveActions :: Problem -> ProblemState -> [Action]
getAllMoveActions problem@(Problem {..}) state@(ProblemState {..}) =
  let robot = problemRobot
      map_ = problemMap
      moves = [
          MUp, MRight, MDown, MLeft,
          MAttachWheels, MAttachDrill, MPlaceBeacon
        ] ++ (MTeleport <$> (S.toList $ robotBeacons robot))
  in moves

getAllActions :: Problem -> ProblemState -> [Action]
getAllActions problem@(Problem {..}) state@(ProblemState {..}) =
  let robot = problemRobot
      map_ = problemMap
      moves = getAllMoveActions problem state
  in moves ++ [MTurnRight, MTurnLeft] ++ (MAttachManipulator <$> (S.toList $ manipulatorExtensionLocations $ robotManipulators robot))

cellsOnMoveLine :: I2 -> I2 -> [I2]
cellsOnMoveLine (V2 x0 y0) (V2 x1 y1)
  | x0 == x1 = if y0 < y1 then [V2 x0 y | y <- [y0..y1]]
                          else [V2 x0 y | y <- [y1..y0]]
  | y0 == y1 = if x0 < x1 then [V2 x y0 | x <- [x0..x1]]
                          else [V2 x y0 | x <- [x1..x0]]
  | otherwise = trace "cellsOnMoveLine: incorrect movement line" $ [V2 x1 y1]

getNeighboursOfType :: Problem -> ProblemState -> [Action] -> [(ProblemState, Action)]
getNeighboursOfType problem@(Problem {..}) state@(ProblemState {..}) moves =
  let robot = problemRobot
      map_ = problemMap
      newRobots = (applyAction robot map_ state) <$> moves
      validRobots = mapMaybe (\(mr, mov) -> case mr of
                                      Just r -> Just (r, mov)
                                      Nothing -> Nothing
                             ) $ zip newRobots moves
      moveSpanCells r = cellsOnMoveLine (robotPosition robot) (robotPosition r)
      validManips r pos = validManipulators map_ pos (robotManipulators r)
      validManipsTotal r = S.toList $ foldr1 (S.union) $ (validManips r) <$> moveSpanCells r
      newWrapped r = map (+ (robotPosition r)) $ validManipsTotal r
      newState r mov = (
          collectBoosters problem (moveSpanCells r) $ state {
            problemRobot = r,
            problemUnwrapped = foldr S.delete problemUnwrapped $ newWrapped r
          }, mov
        )
  in map (\(r, m) -> newState r m) validRobots

getNeighbours :: Problem -> ProblemState -> [(ProblemState, [Action], Int)]
getNeighbours problem@(Problem {..}) state
  | null usefulSteps = moveoutSteps
  | otherwise = take 1 $ sortBy (comparing $ \(s, _, _) -> S.size (problemUnwrapped s) - S.size (problemUnwrapped state)) usefulSteps
  where neighbours = getNeighboursOfType problem state (getAllActions problem state)
        usefulSteps' = filter (\(newState, _) -> S.size (problemUnwrapped newState) /= S.size (problemUnwrapped state)) neighbours
        usefulSteps = map (\(f, s) -> (f, [s], 1)) usefulSteps'

        moveNeighbours state = getNeighboursOfType problem state (getAllMoveActions problem state)
        moveoutSteps = map convertSteps $ maybeToList $ bfs moveNeighbours state hasMovedOut

        hasMovedOut state' = S.size (problemUnwrapped state') /= S.size (problemUnwrapped state)
        convertSteps steps = (finalState, actions, cost)
          where actions = map snd steps
                cost = length steps
                (finalState, _) = last steps

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
