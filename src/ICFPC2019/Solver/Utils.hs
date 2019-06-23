module ICFPC2019.Solver.Utils where

import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Set as S
import qualified Data.Array.Repa as R
import qualified Data.HashMap.Strict as M
import qualified Data.Map.Strict as SM
import Data.Maybe
import Linear.V2

import DNIWEChan.Graph
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

defaultPriorities :: ActionPriority
defaultPriorities = SM.fromList $ [ (MAttachWheels, 20)
                                  , (MPlaceBeacon, 50)
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
                                  ]

getAllMoveActions :: Problem -> ProblemState -> [Action]
getAllMoveActions problem@Problem {..} state@ProblemState {..} =
  let robot = problemRobot
      map_ = problemMap
      moves =
        [ MUp, MRight, MDown, MLeft
        , MAttachWheels
        , MPlaceBeacon
     -- , MAttachDrill
        ] ++ (MTeleport <$> (S.toList $ robotBeacons robot))
  in moves

getAllActions :: Problem -> ProblemState -> [Action]
getAllActions problem@Problem {..} state@ProblemState {..} =
  let robot = problemRobot
      map_ = problemMap
      moves = getAllMoveActions problem state
  in moves
     ++ [MTurnRight, MTurnLeft]
     ++ (MAttachManipulator <$> (S.toList $ manipulatorExtensionLocations $ robotManipulators robot))

getNeighboursOfType :: Problem -> ProblemState -> [Action] -> [(ProblemState, Action)]
getNeighboursOfType problem state = mapMaybe tryMove
  where
    tryMove :: Action -> Maybe (ProblemState, Action)
    tryMove act = case changeState problem state act of
                    Just state' -> Just (state', act)
                    _           -> Nothing

getNeighbours :: ActionPriority -> Problem -> ProblemState -> [(ProblemState, [Action], Int)]
getNeighbours priorities problem@Problem {..} state
  | null usefulSteps = moveoutSteps
  | otherwise = take 1 $ sortBy (comparing $ \(s, _, cost) -> cost - diffWrapped state s) usefulSteps
  where
        neighbours = getNeighboursOfType problem state (getAllActions problem state)
        -- drill requires mutable map!
        defaultCost = 100
        actionPrior act = SM.findWithDefault defaultCost act priorities
        stateUseful newState mov = S.size (problemUnwrapped newState) /= S.size (problemUnwrapped state) || actionPrior mov < defaultCost
        usefulSteps' = filter (uncurry stateUseful) neighbours
        usefulSteps = map (\(f, s) -> (f, [s], actionPrior s)) usefulSteps'

        moveNeighbours state = getNeighboursOfType problem state (getAllMoveActions problem state)
        moveoutSteps = map convertSteps $ maybeToList $ bfs moveNeighbours state hasMovedOut

        hasMovedOut state' = S.size (problemUnwrapped state') /= S.size (problemUnwrapped state)
        convertSteps steps = (finalState, actions, cost)
          where actions = map snd steps
                cost = length steps
                (finalState, _) = last steps

genFinish :: ProblemState -> ProblemState
genFinish start@ProblemState {..} = start {problemUnwrapped = S.empty}

diffWrapped :: ProblemState -> ProblemState -> Int
diffWrapped startState endState = S.size startUnWrapped - S.size endUnWrapped
  where
    startUnWrapped = problemUnwrapped startState
    endUnWrapped = problemUnwrapped endState

addHeuristics :: (ProblemState -> ProblemState -> Int) -> (ProblemState -> ProblemState -> Int) -> ProblemState -> ProblemState -> Int
addHeuristics heur1 heur2 start end = heur1 start end + heur2 start end
