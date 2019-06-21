module ICFPC2019.FastDownward
  ( solveProblem
  , SimpleAction(..)
  , fromSimpleAction
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Linear.V2 (V2(..))
import qualified FastDownward.Exec as FD
import FastDownward hiding (Problem)
import qualified FastDownward as FD
import qualified Data.Array.Repa as R
import ICFPC2019.Types
import ICFPC2019.Utils

-- import Debug.Trace

data SimpleCell = SimpleWrapped
                | SimpleFree
                | SimpleObstacle
                deriving (Show, Eq, Ord)

data SimpleAction = SMUp
                  | SMRight
                  | SMDown
                  | SMLeft
                  | SMNothing
                  deriving (Show, Eq, Ord, Bounded, Enum)

fromSimpleAction :: SimpleAction -> Action
fromSimpleAction SMUp = MUp
fromSimpleAction SMRight = MRight
fromSimpleAction SMDown = MDown
fromSimpleAction SMLeft = MLeft
fromSimpleAction SMNothing = MNothing

simplifyMap :: Set I2 -> MapArray Cell -> MapArray SimpleCell
simplifyMap unwrapped arr = R.computeS $ R.fromFunction (R.extent arr) toSimple
  where toSimple i
          | i `S.member` unwrapped = SimpleFree
          | otherwise =
            case val of
              Free -> SimpleWrapped
              Obstacle -> SimpleObstacle
          where val = arr R.! i

genCells :: MapArray SimpleCell -> FD.Problem (MapArray (Var SimpleCell))
genCells gameMap = mapM newVar gameMap


move :: I2 -> SimpleAction -> I2
-- move idx mov | trace ("move " ++ show idx ++ " " ++ show mov) False = undefined
move (V2 x y) SMUp      = V2 x (y + 1)
move (V2 x y) SMRight   = V2 (x + 1) y
move (V2 x y) SMDown    = V2 x (y - 1)
move (V2 x y) SMLeft    = V2 (x - 1) y
move (V2 x y) SMNothing = V2 x y

isFreeCell :: SimpleCell -> Bool
isFreeCell SimpleFree = True
isFreeCell _          = False

testCell :: MapArray (Var SimpleCell) -> I2 -> Test
-- testCell cells idx | trace ("testCell " ++ show idx) False = undefined
testCell cells idx = cells R.! idx ?= SimpleWrapped


solveProblem :: Problem -> FD.Problem (SolveResult SimpleAction)
solveProblem (Problem {..}) = do
  let currentMap = simplifyMap problemUnwrapped problemMap
  let curSize = R.extent currentMap
  cells <- genCells currentMap
  robotLocation <- newVar (V2 0 0)

  let
    moveRobot :: SimpleAction -> Effect SimpleAction
    moveRobot mov = do
      curLocation <- readVar robotLocation
      let newLocation = move curLocation mov
      guard $ R.inShapeRange (V2 0 0) (curSize - 1) newLocation
      cellE <- readVar (cells R.! newLocation)
      guard $ cellE /= SimpleObstacle
      forM_ (robotManipulators problemRobot) $ \manip -> do
        writeVar (cells R.! (curLocation + manip)) SimpleWrapped
      writeVar robotLocation newLocation
      return mov

  solve
    defaultCfg
    [moveRobot mov | mov <- [SMUp .. SMNothing]]
    $ map (testCell cells) $ S.toList $ problemUnwrapped


lmEval :: FD.Evaluator
lmEval = FD.LMCount FD.LMCountConfiguration
         { lmFactory =
             FD.LMHM FD.LMHMConfiguration
             { m = 2
             , reasonableOrders = False
             , onlyCausalLandmarks = False
             , disjunctiveLandmarks = True
             , conjunctiveLandmarks = True
             , noOrders = False
             }
         , admissible = True
         , optimal = True
         , pref = True
         , alm = True
         , lpSolver = FD.GUROBI
         , transform = FD.NoTransform
         , cacheEstimates = True
         }

lmEval2 ::  FD.Evaluator
lmEval2 = FD.LMCount FD.LMCountConfiguration
          { lmFactory =
            FD.LMExhaust FD.LMExhaustConfiguration
            { reasonableOrders = False
            , onlyCausalLandmarks = False
            , disjunctiveLandmarks = True
            , conjunctiveLandmarks = True
            , noOrders = False
            }
          , admissible = False
          , optimal = False
          , pref = True
          , alm = True
          , lpSolver = FD.GUROBI
          , transform = FD.NoTransform
          , cacheEstimates = True
          }

defaultCfg :: FD.SearchEngine
defaultCfg = astar

astar :: FD.SearchEngine
astar = FD.AStar FD.AStarConfiguration
        { evaluator = lmEval2
        , lazyEvaluator = Nothing --Just lmEval
        , pruning = FD.Null
        , costType = FD.Normal
        , bound = Nothing
        , maxTime = Nothing
        }

wAstar :: FD.SearchEngine
wAstar = FD.LazyWeightedAStar FD.LazyWeightedAStarConfiguration
         { evaluators = [lmEval, lmEval2]
         , preferred = [lmEval]
         , reopenClosed = True
         , boost = 0
         , w = 1
         , randomizeSuccessors = True
         , preferredSuccessorsFirst = False
         , randomSeed = Just 444
         , costType = FD.Normal
         , bound = Nothing
         , maxTime = Nothing
         }

eagerBFS :: FD.SearchEngine
eagerBFS = FD.EagerBestFirst FD.EagerBestFirstConfiguration
           { open = FD.EpsilonGreedy FD.EpsilonGreedyConfiguration
             { eval = lmEval2
             , prefOnly = False
             , epsilon = 0.2
             , randomSeed = Just 444
             }
           , reopenClosed = True
           , fEval = Nothing
           , preferred = [lmEval2]
           , pruning = FD.StubbornSetsSimple FD.StubbornSetsConfiguration
             { minRequiredPruningRatio = 0.0
             , expansionsBeforeCheckingPruningRatio = Just 1000
             }
           , costType = FD.Normal
           , bound = Nothing
           , maxTime = Nothing
           }
  