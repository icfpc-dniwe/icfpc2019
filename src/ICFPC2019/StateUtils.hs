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

cellsOnMoveLine :: I2 -> I2 -> [I2]
cellsOnMoveLine a b
  | a == b = []
  | otherwise = go (a + d)
  where d = signum (b - a)
        go p
          | p == b = [p]
          | otherwise = p : go (p + d)

changeState' :: Problem -> ProblemState -> Robot -> ProblemState
changeState' prob@(Problem {..}) state@(ProblemState {..}) newRobot = checkThings $ foldr collectBoosters newState moveSpanCells
                   
  where moveSpanCells = cellsOnMoveLine (robotPosition problemRobot) (robotPosition newRobot)
        validManips pos = validManipulators problemMap pos (robotManipulators newRobot)
        validManipsTotal = concatMap validManips $ moveSpanCells
        --testCells = S.fromList [V2 16 0, V2 17 0, V2 18 0, V2 16 1, V2 17 1, V2 18 1]
        testCells = S.fromList [V2 x y | x <- [21..33]
                                       , y <- [18..19]
                                       ]
        checkThings a
          -- | not $ S.null $ testCells `S.intersection` validCells =
          | robotPosition newRobot `S.member` testCells =
          -- | robotBoosters newRobot /= robotBoosters problemRobot && robotWheelsLeft problemRobot > 0 =
            trace "" $
            trace ("old robot: " ++ show problemRobot) $
            trace ("new robot: " ++ show newRobot) $
            trace ("new wrapped cells: " ++ show (problemUnwrapped `S.intersection` validCells)) $
            a
          | otherwise = a
          where validCells = S.fromList validManipsTotal
        newState = state { problemRobot = newRobot
                         , problemUnwrapped = foldr S.delete problemUnwrapped validManipsTotal
                         }

changeState :: Problem -> ProblemState -> Action -> Maybe ProblemState
changeState prob@Problem{..} state@(ProblemState {..}) act =
  changeState' prob state <$> applyAction problemMap state act
