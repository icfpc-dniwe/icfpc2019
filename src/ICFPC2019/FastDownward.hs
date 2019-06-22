module ICFPC2019.FastDownward
  ( solveProblem
  ) where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Linear.V2 (V2(..))
import qualified FastDownward.Exec as FD
import FastDownward hiding (Problem)
import qualified FastDownward as FD
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector (V)
import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.RobotUtils

-- import Debug.Trace

data SimpleCell = SimpleWrapped
                | SimpleFree
                deriving (Show, Eq, Ord)

data SimpleAction = SMUp
                  | SMRight
                  | SMDown
                  | SMLeft
                  | SMTurnLeft
                  | SMTurnRight
                  deriving (Show, Eq, Ord, Bounded, Enum)

fromSimpleAction :: SimpleAction -> Action
fromSimpleAction SMUp = MUp
fromSimpleAction SMRight = MRight
fromSimpleAction SMDown = MDown
fromSimpleAction SMLeft = MLeft
fromSimpleAction SMTurnLeft = MTurnLeft
fromSimpleAction SMTurnRight = MTurnRight

type CellArray = R.Array V I2 (Maybe (Var SimpleCell))

genCells :: Set I2 -> MapArray -> FD.Problem CellArray
genCells unwrapped arr = mapM makeOne $ R.computeS $ R.fromFunction (R.extent arr) toSimple
  where toSimple i
          | i `S.member` unwrapped = Just SimpleFree
          | val = Just SimpleWrapped
          | otherwise = Nothing
          where val = arr R.! i

        makeOne Nothing = return Nothing
        makeOne (Just c) = Just <$> newVar c

testCell :: CellArray -> I2 -> Test
testCell cells idx = fromJust (cells R.! idx) ?= SimpleWrapped

solveProblem :: Problem -> ProblemState -> FD.Problem (SolveResult Action)
solveProblem Problem {..} ProblemState {..} = do
  cells <- genCells problemUnwrapped problemMap
  robotLocation <- newVar $ robotPosition problemRobot
  robotOrientation <- newVar N

  let
    checkRange = R.inShapeRange (V2 0 0) (R.extent problemMap - 1)

    markLocations :: Orientation -> I2 -> Effect ()
    markLocations newOrientation newLocation = do
      forM_ (filter checkRange $ map (applyOrientation newOrientation . (newLocation +)) $ S.toList $ robotManipulators problemRobot) $ \wrapped -> do
        case cells R.! wrapped of
          Nothing -> return ()
          Just c -> writeVar c SimpleWrapped

    moveRobot :: (I2 -> I2) -> Effect ()
    moveRobot move = do
      curLocation <- readVar robotLocation
      let newLocation = move curLocation
      guard $ checkRange newLocation
      guard $ isJust $ cells R.! newLocation
      curOrientation <- readVar robotOrientation
      markLocations curOrientation newLocation
      writeVar robotLocation newLocation

    rotateRobot :: (Orientation -> Orientation) -> Effect ()
    rotateRobot rotate = do
      curOrientation <- readVar robotOrientation
      let newOrientation = rotate curOrientation
      curLocation <- readVar robotLocation
      markLocations newOrientation curLocation
      writeVar robotOrientation newOrientation

    applyAction :: SimpleAction -> Effect Action
    applyAction action = do
      case action of
        SMUp    -> moveRobot $ \(V2 x y) -> V2 x       (y + 1)
        SMDown  -> moveRobot $ \(V2 x y) -> V2 x       (y - 1)
        SMLeft  -> moveRobot $ \(V2 x y) -> V2 (x - 1) y
        SMRight -> moveRobot $ \(V2 x y) -> V2 (x + 1) y

        SMTurnLeft -> rotateRobot rotateLeft
        SMTurnRight -> rotateRobot rotateRight

      return $ fromSimpleAction action

  solve
    defaultCfg
    [applyAction mov | mov <- [minBound .. maxBound]]
    $ map (testCell cells) $ S.toList $ problemUnwrapped


lmEval :: FD.Evaluator
lmEval = FD.LMCount FD.LMCountConfiguration
         { lmFactory =
             FD.LMHM FD.LMHMConfiguration
             { m = 2
             , reasonableOrders = True
             , onlyCausalLandmarks = False
             , disjunctiveLandmarks = True
             , conjunctiveLandmarks = True
             , noOrders = False
             }
         , admissible = False
         , optimal = False
         , pref = True
         , alm = True
         , lpSolver = FD.CPLEX
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
          , lpSolver = FD.CPLEX
          , transform = FD.NoTransform
          , cacheEstimates = True
          }

goalCountEval ::  FD.Evaluator
goalCountEval = FD.GoalCount FD.GoalCountConfiguration
          { transform = FD.NoTransform
          , cacheEstimates = True
          }

mergeShrink ::  FD.Evaluator
mergeShrink = FD.MergeAndShrink FD.MergeAndShrinkConfiguration
  { transform = FD.NoTransform
  , cacheEstimates = True
  , mergeStrategy = FD.MergeSCCs FD.MergeSCCsConfiguration
    { orderOfSCCs = FD.Topological
    , mergeTree = Nothing
    , mergeSelector = Just $ FD.ScoreBasedFiltering
      [ FD.GoalRelevance
      , FD.DFP
      , FD.TotalOrder FD.TotalOrderConfiguration
        { atomicTsOrder = FD.ReverseLevelAtomicTs
        , productTsOrder = FD.NewToOld
        , atomicBeforeProduct = False
        , randomSeed = Just 444
        }
      ]
    }
  , shrinkStrategy = FD.Bisimulation FD.BisimulationConfiguration
    { greedy = False
    , atLimit = FD.UseUp
    }
  , labelReduction = FD.ExactGeneralizedLabelReduction FD.ExactGeneralizedLabelReductionConfiguration
    { beforeShrinking = False
    , beforeMerging = False
    , method = FD.AllTransitionSystemsWithFixpoint
    , systemOrder = FD.RandomSystemOrder
    , randomSeed = Just 444
    }
  , pruneUnreachableStates = True
  , pruneIrrelevantStates = True
  , maxStates = Nothing
  , maxStatesBeforeMerge = Nothing
  , thresholdBeforeMerge = Nothing
  , verbosity = FD.Silent
  }

defaultCfg :: FD.SearchEngine
defaultCfg = wAstar

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
         { evaluators = [lmEval, goalCountEval]
         , preferred = [goalCountEval]
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

eagerBFS2 :: FD.SearchEngine
eagerBFS2 = FD.EagerBestFirst FD.EagerBestFirstConfiguration
           { open = FD.EpsilonGreedy FD.EpsilonGreedyConfiguration
             { eval = lmEval2
             , prefOnly = False
             , epsilon = 0.2
             , randomSeed = Just 444
             }
           , reopenClosed = True
           , fEval = Nothing
           , preferred = [mergeShrink, lmEval2]
           , pruning = FD.StubbornSetsSimple FD.StubbornSetsConfiguration
             { minRequiredPruningRatio = 0.0
             , expansionsBeforeCheckingPruningRatio = Just 1000
             }
           , costType = FD.Normal
           , bound = Nothing
           , maxTime = Nothing
           }

  
