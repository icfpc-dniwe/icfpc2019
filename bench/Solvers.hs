import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Linear.V3 (V3(..))
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import Criterion.Main

import ICFPC2019.Types
import ICFPC2019.IO
import ICFPC2019.Raw
import qualified ICFPC2019.Solver.AStar as SA

setupEnv :: IO (Problem, ProblemState)
setupEnv = do
  input <- BL.readFile "problems/prob-020.desc"
  rawProb <-
    case parse rawProblem input of
      Done _ r -> return r
      Fail _ ctx e -> fail ("Failed to parse in " ++ show ctx ++ ": " ++ e)
  return $ convertProblem rawProb

main :: IO ()
main = defaultMain
  [ env setupEnv $ \ ~(problem, state) -> bgroup "Solvers"
    [ bench "AStar" $ nf (SA.solve problem) state
    ]
  ]

