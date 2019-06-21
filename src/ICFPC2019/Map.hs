module ICFPC2019.Map where

--import Data.Set
import Control.Monad
import Linear.V3 (V3(..))
import Linear.V2 (V2(..))
import qualified FastDownward.Exec as Exec
import FastDownward
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector
import qualified Data.Array.Repa.Eval as RE
import ICFPC2019.Types (I2)
import ICFPC2019.Utils

-- import Debug.Trace

data Cell = Wrapped
          | Free
          | Obstacle
          deriving (Show, Eq, Ord)

data RobotOrientation = North | East | South | West
  deriving (Show, Eq, Ord)

data Movement = MUp | MRight | MDown | MLeft | MNothing
  deriving (Show, Eq, Ord, Bounded, Enum)

data Booster = Extension
             | FastWheels
             | Drill
             | Mysterious
             deriving (Show, Eq, Ord)

type Map = R.Array V I2 Cell

defaultMap :: Map
defaultMap = RE.fromList (V2 3 4) [ Free, Free, Free
                                  , Free, Obstacle, Free
                                  , Free, Obstacle, Free
                                  , Free, Free, Free
                                  ]


genCells :: Map -> Problem (R.Array V I2 (Var Cell))
genCells gameMap = mapM newVar gameMap

move :: I2 -> Movement -> I2
-- move idx mov | trace ("move " ++ show idx ++ " " ++ show mov) False = undefined
move (V2 x y) MUp    = V2 x (y + 1)
move (V2 x y) MRight = V2 (x + 1) y
move (V2 x y) MDown  = V2 x (y - 1)
move (V2 x y) MLeft  = V2 (x - 1) y
move (V2 x y) _      = V2 x y


freeIdx :: Map -> [I2]
freeIdx gameMap = filter (\idx -> (gameMap R.! idx) == Free) $ map (\idx -> R.fromIndex curShape idx) [0 .. (R.size curShape - 1)]
  where
    curShape = R.extent gameMap


testCell :: R.Array V I2 (Var Cell) -> I2 -> Test
-- testCell cells idx | trace ("testCell " ++ show idx) False = undefined
testCell cells idx = cells R.! idx ?= Wrapped


problem :: Problem (SolveResult Movement)
problem = do
  let currentMap = defaultMap
  let curSize = R.extent currentMap
  cells <- genCells currentMap
  robotLocation <- newVar (V2 0 0)
  robotOrientation <- newVar North

  let
    moveRobot :: Movement -> Effect Movement
    moveRobot mov = do
      curLocation <- readVar robotLocation
      writeVar (cells R.! curLocation) Wrapped
      let newLocation = move curLocation mov
      guard $ R.inShapeRange (V2 0 0) (curSize - 1) newLocation
      cellE <- readVar (cells R.! newLocation)
      guard $ cellE /= Obstacle
      writeVar robotLocation newLocation
      writeVar (cells R.! newLocation) Wrapped
      return mov

  solve
    cfg
    [moveRobot mov | mov <- [MUp .. MNothing]]
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


cfg :: Exec.SearchEngine
cfg =
  Exec.AStar Exec.AStarConfiguration
    { evaluator =
        Exec.LMCount Exec.LMCountConfiguration
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
          , lpSolver = Exec.CLP
          , transform = Exec.NoTransform
          , cacheEstimates = True
          }
    , lazyEvaluator = Nothing
    , pruning = Exec.Null
    , costType = Exec.Normal
    , bound = Nothing
    , maxTime = Nothing
}
