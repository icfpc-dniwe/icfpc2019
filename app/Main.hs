module Main where

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

import qualified ICFPC2019.Solver.AStar as SA
import qualified ICFPC2019.Solver.BFS as SB

solveFD :: Problem -> ProblemState -> IO [Action]
solveFD prob state = do
  res <- FD.runProblem (solveProblem prob state)
  
  case res of
    FD.Solved plan -> return $ FD.totallyOrderedPlan plan
    _ -> fail "Couldn't find a plan!"

solveSA :: Problem -> ProblemState -> IO [Action]
solveSA prob state = do
  let res = SA.solve prob state

  case res of
    Just plan -> return $ map snd plan
    _ -> fail "Couldn't find a plan!"

solveSB :: Problem -> ProblemState -> IO [Action]
solveSB prob state = do
  let res = SB.solve prob state

  case res of
    Just plan -> return $ map snd plan
    _ -> fail "Couldn't find a plan!"

main :: IO ()
main = do
  [path] <- getArgs
  input <- BL.readFile path
  rawProb <-
    case parse rawProblem input of
      Done _ r -> return r
      Fail _ ctx e -> fail ("Failed to parse in " ++ show ctx ++ ": " ++ e)
  hPutStrLn stderr $ show rawProb
  let (prob, state) = convertProblem rawProb
  hPutStrLn stderr $ showPlane $ problemMap prob
  solution <- solveSA prob state
  --solution <- solveFD prob state
  hPutStrLn stderr $ "Found solution, length " ++ show (length solution)

  BB.hPutBuilder stdout $ buildSolution solution
  putStrLn ""
