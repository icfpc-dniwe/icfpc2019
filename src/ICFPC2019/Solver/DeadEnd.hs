module ICFPC2019.Solver.DeadEnd where

import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe

import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.RobotUtils
import ICFPC2019.StateUtils
import ICFPC2019.Solver.Utils

import DNIWEChan.Graph
import DNIWEChan.Metric

import Debug.Trace

solve :: Solver -> RawProblem -> Problem -> ProblemState -> Maybe [(ProblemState, [Action])]
solve nextSolve RawProblem{..} prob@Problem{..} state@ProblemState{..} =
  let
    state' = if null steps
             then state
             else fst $ last steps
    steps = map (\(s, a) -> (s, [a])) $ traverseWaypoints prob state $ trace ("reor " ++ show points) points
    points = reorderPoints startPoint waypoints
    startPoint = findClosest (getRobotPos state) $ trace ("www " ++ show waypoints) waypoints
    waypoints = mapMaybe (findClosestFree prob state) $ trace ("raw " ++ show rawMap) rawMap
  in
    case nextSolve prob state' of
      Just additionalSteps -> Just $ steps ++ additionalSteps
      Nothing              -> Nothing

traverseWaypoints :: Problem -> ProblemState -> [I2] -> [(ProblemState, Action)]
traverseWaypoints prob state []                = []
traverseWaypoints prob state (waypoint:others) = steps ++ traverseWaypoints prob state' others
  where
    state' = if null steps
             then state
             else fst $ last steps
    steps = fromMaybe [] $ goToWaypoint prob state waypoint

goToWaypoint :: Problem -> ProblemState -> I2 -> Maybe [(ProblemState, Action)]
goToWaypoint problem state waypoint | trace ("goTo " ++ show waypoint) False = undefined
goToWaypoint problem state waypoint = aStar (map (\(s, acts, c) -> (s, head acts, c)) . getNeighbours defaultPriorities problem 0)
                                            (mlenDistance waypoint . getRobotPos)
                                            state
                                            (\s -> not $ S.member waypoint $ problemUnwrapped s)

reorderPoints :: I2 -> [I2] -> [I2]
reorderPoints start points = helper start points []
  where
    helper :: I2 -> [I2] -> [I2] -> [I2]
--    helper st wr tr | trace ("reordeing " ++ show st ++ " <| " ++ show wr ++ " |> " ++ show tr) False = undefined
    helper st [] tr = tr
    helper st (point:others) tr | st == point = (point:others) ++ tr
                                | otherwise   = helper st others (tr ++ [point])

findClosest :: I2 -> [I2] -> I2
--findClosest pos points | trace ("findClosest " ++ show pos ++ " <| " ++ show points) False = undefined
findClosest pos points = fst $ foldr1 chooseMin $ map (\x -> (x, mlenDistance pos x)) points
  where
--    chooseMin (point1, dist1) (point2, dist2) | trace ("choose " ++ show point1 ++ " " ++ show dist1 ++ " vs " ++ show point2 ++ " " ++ show dist2) False = undefined
    chooseMin (point1, dist1) (point2, dist2) | dist1 < dist2 = (point1, dist1)
                                              | otherwise     = (point2, dist2)

findClosestFree :: Problem -> ProblemState -> I2 -> Maybe I2
findClosestFree Problem{..} ProblemState{..} pos =
  bfs' (getMapNeighbours problemMap problemDrilled) pos isUnWrapped
    where
      isUnWrapped point = S.member point problemUnwrapped
