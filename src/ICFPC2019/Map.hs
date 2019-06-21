module ICFPC2019.Map where

import qualified Data.Set as S
import Control.Monad
import Linear.V3 (V3(..))
import Linear.V2 (V2(..))
import qualified FastDownward.Exec as Exec
import FastDownward
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector
import qualified Data.Array.Repa.Eval as RE
import ICFPC2019.Types hiding (Problem)
import ICFPC2019.Utils

-- import Debug.Trace

data SimpleCell = SimpleWrapped
                | SimpleFree
                | SimpleObstacle
                deriving (Show, Eq, Ord)

data SimpleAction = SMUp | SMRight | SMDown | SMLeft | SMNothing
  deriving (Show, Eq, Ord, Bounded, Enum)

type Map = MapArr SimpleCell

--defaultMap :: Map
--defaultMap = RE.fromList (V2 3 4) [ Free, Free, Free
--                                  , Free, Obstacle, Free
--                                  , Free, Obstacle, Free
--                                  , Free, Free, Free
--                                  ]

defaultMap :: MapArr Cell
defaultMap = R.computeS $ R.fromFunction (V2 10 10) (\_ -> Free {cellWrapped = False, cellObjects = S.empty})

toSimple :: Cell -> SimpleCell
toSimple Obstacle = SimpleObstacle
toSimple Free {cellWrapped = True} = SimpleWrapped
toSimple Free {cellWrapped = False} = SimpleFree

simplifyMap :: MapArr Cell -> Map
simplifyMap = fmap toSimple

genCells :: Map -> Problem (MapArr (Var SimpleCell))
genCells gameMap = mapM newVar gameMap


move :: I2 -> SimpleAction -> I2
-- move idx mov | trace ("move " ++ show idx ++ " " ++ show mov) False = undefined
move (V2 x y) SMUp    = V2 x (y + 1)
move (V2 x y) SMRight = V2 (x + 1) y
move (V2 x y) SMDown  = V2 x (y - 1)
move (V2 x y) SMLeft  = V2 (x - 1) y
move (V2 x y) _       = V2 x y

isFreeCell :: SimpleCell -> Bool
isFreeCell SimpleFree = True
isFreeCell _          = False

freeIdx :: Map -> [I2]
freeIdx gameMap = filter (\idx -> isFreeCell $ gameMap R.! idx) $ map (R.fromIndex curShape) [0 .. (R.size curShape - 1)]
  where
    curShape = R.extent gameMap


testCell :: MapArr (Var SimpleCell) -> I2 -> Test
-- testCell cells idx | trace ("testCell " ++ show idx) False = undefined
testCell cells idx = cells R.! idx ?= SimpleWrapped


problem :: Problem (SolveResult SimpleAction)
problem = do
  let currentMap = simplifyMap $ defaultMap
  let curSize = R.extent currentMap
  cells <- genCells currentMap
  robotLocation <- newVar (V2 0 0)

  let
    moveRobot :: SimpleAction -> Effect SimpleAction
    moveRobot mov = do
      curLocation <- readVar robotLocation
      writeVar (cells R.! curLocation) SimpleWrapped
      let newLocation = move curLocation mov
      guard $ R.inShapeRange (V2 0 0) (curSize - 1) newLocation
      cellE <- readVar (cells R.! newLocation)
      guard $ cellE /= SimpleObstacle
      writeVar robotLocation newLocation
      writeVar (cells R.! newLocation) SimpleWrapped
      return mov

  solve
    cfg
    [moveRobot mov | mov <- [SMUp .. SMNothing]]
    $ map (testCell cells) $ freeIdx currentMap


runMain :: IO ()
runMain = do
  res <- runProblem problem
  case res of
    Solved plan -> do
      putStrLn "Found a plan!"
      zipWithM_
        ( \i step -> putStrLn ( show i ++ ": " ++ show step ) )
        [ 1::Int .. ]
        ( totallyOrderedPlan plan )

    _ ->
      putStrLn "Couldn't find a plan!"


lmEval :: Exec.Evaluator
lmEval = Exec.LMCount Exec.LMCountConfiguration
                        { lmFactory =
                            Exec.LMHM Exec.LMHMConfiguration
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
                        , lpSolver = Exec.GUROBI
                        , transform = Exec.NoTransform
                        , cacheEstimates = True
                        }

lmEval2 = Exec.LMCount Exec.LMCountConfiguration
                    { lmFactory =
                        Exec.LMExhaust Exec.LMExhaustConfiguration
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
                    , lpSolver = Exec.GUROBI
                    , transform = Exec.NoTransform
                    , cacheEstimates = True
                    }


cfg :: Exec.SearchEngine
cfg = astar

astar =   Exec.AStar Exec.AStarConfiguration
    { evaluator = lmEval2
    , lazyEvaluator = Nothing --Just lmEval
    , pruning = Exec.Null
    , costType = Exec.Normal
    , bound = Nothing
    , maxTime = Nothing
    }

wAstar =   Exec.LazyWeightedAStar Exec.LazyWeightedAStarConfiguration
    { evaluators = [lmEval, lmEval2]
    , preferred = [lmEval]
    , reopenClosed = True
    , boost = 0
    , w = 1
    , randomizeSuccessors = True
    , preferredSuccessorsFirst = False
    , randomSeed = Just 444
    , costType = Exec.Normal
    , bound = Nothing
    , maxTime = Nothing
    }

eagerBFS = Exec.EagerBestFirst Exec.EagerBestFirstConfiguration
  { open = Exec.EpsilonGreedy Exec.EpsilonGreedyConfiguration
    { eval = lmEval2
    , prefOnly = False
    , epsilon = 0.2
    , randomSeed = Just 444
    }
  , reopenClosed = True
  , fEval = Nothing
  , preferred = [lmEval2]
  , pruning = Exec.StubbornSetsSimple Exec.StubbornSetsConfiguration
    { minRequiredPruningRatio = 0.0
    , expansionsBeforeCheckingPruningRatio = Just 1000
    }
  , costType = Exec.Normal
  , bound = Nothing
  , maxTime = Nothing
  }
