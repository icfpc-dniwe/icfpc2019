module ICFPC2019.StateUtils where

import qualified Data.HashMap.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2
import ICFPC2019.Utils
import ICFPC2019.Types
import ICFPC2019.RobotUtils

collectBooster :: Problem -> ProblemState -> Booster -> ProblemState
collectBooster problem@Problem {..} state@ProblemState {..} bst =
  let robot = problemRobot
      map_ = problemMap
      executeAction action =
        let actionResult = applyPick robot map_ state action
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
collectBoosters problem@Problem {..} (h:t) state@ProblemState {..} = collectBoosters problem t $
  let robot = problemRobot
      pos = h
      collectAt pos st = collectBoosters' problem (S.toList $ problemBoosters M.! pos) st
      removeAt pos st = st { problemBoosters = M.delete pos problemBoosters }
  in
    if M.member pos problemBoosters
      then removeAt pos $ collectAt pos state
      else state

cellsOnMoveLine :: I2 -> I2 -> [I2]
cellsOnMoveLine (V2 x0 y0) (V2 x1 y1)
  | x0 == x1 = if y0 < y1 then [V2 x0 y | y <- [y0..y1]]
                          else reverse [V2 x0 y | y <- [y1..y0]]
  | y0 == y1 = if x0 < x1 then [V2 x y0 | x <- [x0..x1]]
                          else reverse [V2 x y0 | x <- [x1..x0]]
  | otherwise = error $ "cellsOnMoveLine: incorrect movement line" ++ show [V2 x1 y1]


changeState :: Problem -> ProblemState -> Action -> Maybe ProblemState
changeState prob@Problem{..} state@ProblemState{..} act =
  case applyAction problemRobot problemMap state act of
    Just newRobot -> if isValidRobot prob newRobot
                     then Just $ newState newRobot
                     else Nothing
      where
        moveSpanCells r = cellsOnMoveLine (robotPosition problemRobot) (robotPosition r)
        validManips r pos = validManipulators problemMap pos (robotManipulators r)
        validManipsTotal r = S.toList $ foldr S.union S.empty $ validManips r <$> moveSpanCells r
        newWrapped r = map (+ robotPosition r) $ validManipsTotal r
        newState r = collectBoosters prob (moveSpanCells r) $
                     state {problemRobot = r
                           , problemUnwrapped = foldr S.delete problemUnwrapped $ newWrapped r
                           }
    _             -> Nothing