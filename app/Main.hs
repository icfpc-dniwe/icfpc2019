module Main where

import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import System.Environment
import System.IO
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import qualified FastDownward as FD

import ICFPC2019.Types
import ICFPC2019.Raw
import ICFPC2019.IO
import ICFPC2019.Visualize
import ICFPC2019.FastDownward
import ICFPC2019.Skeletonize

import qualified ICFPC2019.Solver.AStar as SA
import qualified ICFPC2019.Solver.BFS as SB
import qualified ICFPC2019.Solver.DFS as SD
import qualified ICFPC2019.Solver.Skeleton as SS
import qualified ICFPC2019.Solver.DeadEnd as SED
import ICFPC2019.Validate

solveFD :: Problem -> ProblemState -> IO [Action]
solveFD prob state = do
  res <- FD.runProblem (solveProblem prob state)
  
  case res of
    FD.Solved plan -> return $ FD.totallyOrderedPlan plan
    FD.Unsolvable -> fail "Unsolvable!"
    FD.Crashed a b ec -> fail $ "Couldn't find a plan: " ++ a ++ " " ++ b ++ " " ++ show ec

solveSA :: Problem -> ProblemState -> IO [Action]
solveSA prob state = do
  let res = SA.solve prob state

  case res of
    Just plan -> return $ concatMap snd plan
    _ -> fail "Unsolvable!"

solveSB :: Problem -> ProblemState -> IO [Action]
solveSB prob state = do
  let res = SB.solve prob state

  case res of
    Just plan -> return $ concatMap snd plan
    _ -> fail "Unsolvable!"

solveSS :: Problem -> ProblemState -> IO [Action]
solveSS prob state = do
  coreNodes <- getCoreNodes $ problemMap prob
  let clusters = convertSkeleton (problemMap prob) coreNodes
  hPutStrLn stderr $ "Clusters count: " ++ show (M.size clusters)
  let res = SS.solve prob clusters state

  case res of
    Just plan -> return $ map snd plan
    _ -> fail "Unsolvable!"

solveSD :: Problem -> ProblemState -> IO [Action]
solveSD prob state = do
  let res = SD.solve prob state

  case res of
    Just plan -> return $ concatMap snd plan
    _ -> fail "Unsolvable!"

solveSED :: RawProblem -> Problem -> ProblemState -> IO [Action]
solveSED rawProb prob state = do
  let res = SED.solve SA.solve rawProb prob state

  case res of
    Just plan -> return $ concatMap snd plan
    _ -> fail "Unsolvable!"

main :: IO ()
main = do
  --path <- getEnv "PATH"
  --putStrLn ("PATH=" ++ path)
  [path] <- getArgs
  hPutStrLn stderr $ "Running task: " ++ show path
  input <- BL.readFile path
  rawProb <-
    case parse rawProblem input of
      Done _ r -> return r
      Fail _ ctx e -> fail ("Failed to parse in " ++ show ctx ++ ": " ++ e)
  --hPutStrLn stderr $ show rawProb
  let (prob, state) = convertProblem rawProb
--  hPutStrLn stderr $ showPlane $ problemMap prob

--  solution <- solveSED rawProb prob state
  --solution <- solveSS prob state
  solution <- solveSA prob state
  --solution <- solveFD prob state
  hPutStrLn stderr $ "Found solution, length " ++ show (length solution)
  --printPath prob state solution

  BB.hPutBuilder stdout $ buildSolution solution
  putStrLn ""
