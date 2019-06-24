module ICFPC2019.StateUtils where

import qualified Data.HashMap.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2
import Debug.Trace

import ICFPC2019.Utils
import ICFPC2019.Types
import ICFPC2019.RobotUtils

collectBoosters ::  I2 -> ProblemState -> ProblemState
collectBoosters p state@(ProblemState {..}) =
  let (foundBoosters, newBoosters) =
        case M.lookup p problemBoosters of
          Nothing -> (S.empty, problemBoosters)
          Just boosters -> (boosters, M.delete p problemBoosters)

      addBooster booster robot = robot { robotBoosters = M.insertWith (+) booster 1 $ robotBoosters robot }
  
      newRobot = foldr addBooster problemRobot foundBoosters

  in state { problemBoosters = newBoosters
           , problemRobot = newRobot
           }

changeState' :: Problem -> ProblemState -> ([I2], Robot) -> ProblemState
changeState' prob@(Problem {..}) state@(ProblemState {..}) (moveSpanCells, newRobot) = foldr collectBoosters newState moveSpanCells
  where validManips pos = validManipulators problemMap problemDrilled pos (robotManipulators newRobot)
        validManipsTotal = concatMap validManips $ moveSpanCells
        newDrilled =
            if drillEnabled problemRobot
                then foldr S.insert problemDrilled moveSpanCells
                else problemDrilled
        newState = state { problemRobot = newRobot
                         , problemUnwrapped = foldr S.delete problemUnwrapped validManipsTotal
                         , problemDrilled = newDrilled
                         }

changeState :: Problem -> ProblemState -> Action -> Maybe ProblemState
changeState prob@Problem{..} state@(ProblemState {..}) act =
  changeState' prob state <$> applyAction problemMap state act
