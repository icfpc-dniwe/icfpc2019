module ICFPC2019.Solver.Utils where

import Data.Maybe
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Array.Repa as R
import qualified Data.HashMap.Strict as M
import qualified Data.Map.Strict as SM
import Data.Maybe
import Linear.V2

import DNIWEChan.Graph
import DNIWEChan.Metric
import ICFPC2019.Utils
import ICFPC2019.Types
import ICFPC2019.RobotUtils
import ICFPC2019.StateUtils

import Debug.Trace

isBooster :: Action -> Bool
isBooster (MAttachManipulator _) = True
isBooster MAttachWheels = True
isBooster MAttachDrill = True
isBooster MPlaceBeacon = True
isBooster _ = False

isRotation :: Action -> Bool
isRotation MTurnRight = True
isRotation MTurnLeft = True
isRotation _ = False

defaultActionCost :: Int
defaultActionCost = 100

defaultPriorities :: ActionPriority
defaultPriorities = SM.fromList $ [ (MAttachWheels, 20)
                                  , (MAttachDrill, 150)
                                  --, (MPlaceBeacon, 50)
                                  ] ++
                                  [ (MAttachManipulator pos, 10 + idx)
                                    | (idx, pos) <- zip [1 ..]
                                      [ V2 0 1
                                      , V2 0 (-1)
                                      , V2 1 2
                                      , V2 1 (-2)
                                      , V2 0 2
                                      , V2 0 (-2)
                                      , V2 1 3
                                      , V2 1 (-3)
                                      , V2 0 3
                                      , V2 0 (-3)
                                      ]
                                  ] ++
                                  [
                                    (MTurnRight, 60)
                                  , (MTurnLeft, 60)
                                  ]

activePriorities :: ActionPriority -> ProblemState -> Action -> Int
activePriorities priors ProblemState{..} MAttachDrill
  | robotDrillLeft problemRobot > 0  = defaultActionCost
  | otherwise                        = SM.findWithDefault defaultActionCost MAttachDrill priors
activePriorities priors ProblemState{..} MAttachWheels
  | robotWheelsLeft problemRobot > 0  = defaultActionCost
  | otherwise                         = SM.findWithDefault defaultActionCost MAttachWheels priors
activePriorities priors _ act = SM.findWithDefault defaultActionCost act priors

getAllMoveActions' :: Problem -> ProblemState -> [Action]
getAllMoveActions' problem@Problem {..} state@ProblemState {..} =
  let robot = problemRobot
      moves =
        [ MUp, MRight, MDown, MLeft
        , MAttachWheels
        --, MPlaceBeacon
        --, MAttachDrill
        ] -- ++ (MTeleport <$> (S.toList $ robotBeacons robot))
  in moves

getAllMoveActions :: Problem -> ProblemState -> [Action]
getAllMoveActions problem state = getAllMoveActions' problem state

addUpDown :: Set I2 -> [I2]
addUpDown pts = [V2 xMin (yMin - 1), V2 xMax (yMax + 1)]
  where (V2 xMin yMin) = minimumBy (comparing $ \(V2 _ y) -> y) $ S.toList pts
        (V2 xMax yMax) = maximumBy (comparing $ \(V2 _ y) -> y) $ S.toList pts

addLeftRight :: Set I2 -> [I2]
addLeftRight pts = [V2 (xMin - 1) yMin, V2 (xMax + 1) yMax]
  where (V2 xMin yMin) = minimumBy (comparing $ \(V2 x _) -> x) $ S.toList pts
        (V2 xMax yMax) = maximumBy (comparing $ \(V2 x _) -> x) $ S.toList pts

optimizedExtension :: Orientation -> Set I2 -> [I2]
optimizedExtension E = addUpDown
optimizedExtension W = addUpDown
optimizedExtension N = addLeftRight
optimizedExtension S = addLeftRight

getAllActions :: Problem -> ProblemState -> [Action]
getAllActions problem@Problem {..} state@ProblemState {..} =
  let robot = problemRobot
      moves = getAllMoveActions' problem state
  in moves
     ++ [MTurnRight, MTurnLeft] --, MPlaceBeacon]
     ++ (MAttachManipulator <$> (optimizedExtension (robotOrientation robot) $ robotManipulators robot))

getNeighboursOfType :: Problem -> ProblemState -> [Action] -> [(ProblemState, Action)]
getNeighboursOfType problem state = mapMaybe tryMove
  where
    tryMove :: Action -> Maybe (ProblemState, Action)
    tryMove act = case changeState problem state act of
                    Just state' -> Just (state', act)
                    _           -> Nothing

findFreePoint :: Problem -> ProblemState -> Maybe I2
findFreePoint Problem{..} state@ProblemState{..} = bfs' (getMapNeighbours problemMap problemDrilled) startPos isUnWrapped
  where
    startPos = robotPosition problemRobot
    isUnWrapped point = S.member point problemUnwrapped

moveOutActionPrior :: Action -> Int
moveOutActionPrior MAttachDrill = 10
moveOutActionPrior _            = 1

goToPoint :: Problem -> ProblemState -> I2 -> Maybe [(ProblemState, Action)]
goToPoint prob state point = aStar (map (\(s, a) -> (s, a, moveOutActionPrior a)) . moveNeighbours prob) (mlenDistance point . getRobotPos) state (hasMovedOut state)

getRobotPos :: ProblemState -> I2
getRobotPos = robotPosition . problemRobot

moveNeighbours :: Problem -> ProblemState -> [(ProblemState, Action)]
moveNeighbours problem state = getNeighboursOfType problem state (getAllMoveActions problem state)

moveoutSteps :: Problem -> ProblemState -> [(ProblemState, [Action], Int)]
moveoutSteps problem state = case findFreePoint problem state of
  Nothing    -> []
  Just point -> map convertSteps $ maybeToList $ goToPoint problem state point

convertSteps :: [(ProblemState, Action)] -> (ProblemState, [Action], Int)
convertSteps steps = (finalState, actions, cost)
  where actions = map snd steps
        cost = length steps
        (finalState, _) = last steps

hasMovedOut :: ProblemState -> ProblemState -> Bool
hasMovedOut state state' = S.size (problemUnwrapped state') /= S.size (problemUnwrapped state)

getNeighbours :: ActionPriority -> Problem -> Int -> ProblemState -> [(ProblemState, [Action], Int)]
getNeighbours priorities problem@Problem {..} depth state
--  | trace ("usef " ++ show (length usefulSteps)) False = undefined
  | hasBoostersInProximity 4 = collectBoosterSteps $ head $ visibleBoostersInProximity 4
  | null usefulSteps = moveoutSteps problem state
  | otherwise = take 1 $ sortBy (comparing $ \(s, _, cost) -> cost - cellPrior s) usefulSteps
  where
        neighbours = getNeighboursOfType problem state (getAllActions problem state)
        nextBestCost :: ProblemState -> Int
--        nextBestCost s' | trace ("nextBest " ++ show depth ++ " p " ++ show (robotPosition $ problemRobot s')) False = undefined
        nextBestCost s' = if depth < 1
                          then case getNeighbours priorities problem (depth + 1) s' of
                            [] -> -1000 * depth
                            elems -> minimum . map (\(_, _, c') -> c') $ elems
                          else defaultActionCost
        actionPrior = activePriorities priorities state
        drilledCells s = problemDrilled s
        wrappedCells s s' = problemUnwrapped s' S.\\ problemUnwrapped s
        cellPrior s = sum $ map (\x -> 1 + x * 3) $ numWalls problemMap (drilledCells s) <$> (S.toList $ wrappedCells state s)
        stateUseful newState mov =
          let wrapping = S.size (wrappedCells newState state) /= 0
              prioritized = actionPrior mov < defaultActionCost
              robotPosChanging = getRobotPos newState /= getRobotPos state
              rotation = isRotation mov
              ----
              useful = wrapping || prioritized
              useless = not wrapping && not robotPosChanging && rotation
          in
            useful && not useless
        usefulSteps' = filter (uncurry stateUseful) neighbours
        usefulSteps = map (\(f, s) -> (f, [s], nextBestCost f + actionPrior s)) usefulSteps'

        visibleBoostersInProximity maxDest =
          let rpos = robotPosition $ problemRobot state
              delta = (V2 maxDest maxDest)
              boosters = boostersInBoundingBox problemMap state (rpos - delta) $ rpos + delta
          in filter (\bpos -> checkCellVisibility problemMap (drilledCells state) rpos bpos) $ S.toList boosters
        hasBoostersInProximity maxDest = any (>0) $ visibleBoostersInProximity maxDest
        moveNeighbours' = moveNeighbours problem
        collectBoosterSteps boosterPos = map convertSteps $ maybeToList $ bfs moveNeighbours' state $ hasCollectedBooster boosterPos
        hasCollectedBooster pos state' = M.member pos (problemBoosters state') /= M.member pos (problemBoosters state)

genFinish :: ProblemState -> ProblemState
genFinish start@ProblemState {..} = start {problemUnwrapped = S.empty}

diffWrapped :: ProblemState -> ProblemState -> Int
diffWrapped startState endState = S.size startUnWrapped - S.size endUnWrapped
  where
    startUnWrapped = problemUnwrapped startState
    endUnWrapped = problemUnwrapped endState

addHeuristics :: (ProblemState -> ProblemState -> Int) -> (ProblemState -> ProblemState -> Int) -> ProblemState -> ProblemState -> Int
addHeuristics heur1 heur2 start end = heur1 start end + heur2 start end
