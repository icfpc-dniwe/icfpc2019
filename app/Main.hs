module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)

import ICFPC2019.Types
import ICFPC2019.Raw
import ICFPC2019.IO
import ICFPC2019.Visualize

main :: IO ()
main = do
  [path] <- getArgs
  input <- BL.readFile path
  rawProb <-
    case parse rawProblem input of
      Done _ r -> return r
      Fail _ ctx e -> fail ("Failed to parse in " ++ show ctx ++ ": " ++ e)
  print rawProb
  let prob = convertProblem rawProb
  putStrLn $ showPlane $ problemMap prob
