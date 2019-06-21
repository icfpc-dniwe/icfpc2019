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

main :: IO ()
main = do
  [path] <- getArgs
  input <- BL.readFile path
  rawProb <-
    case parse rawProblem input of
      Done _ r -> return r
      Fail _ ctx e -> fail ("Failed to parse in " ++ show ctx ++ ": " ++ e)
  print rawProb
  let (prob, state) = convertProblem rawProb
  putStrLn $ showPlane $ problemMap prob
--  res <- FD.runProblem (solveProblem prob state)
--
--  solution <-
--    case res of
--      FD.Solved plan -> return $ map fromSimpleAction $ FD.totallyOrderedPlan plan
--      _ -> fail "Couldn't find a plan!"

  let res = SA.solve prob state

  solution <-
    case res of
      Just plan -> return $ map snd plan
      _ -> fail "Couldn't find a plan!"

  BB.hPutBuilder stdout $ buildSolution solution
  putStrLn ""
