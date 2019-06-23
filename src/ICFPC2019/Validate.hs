module ICFPC2019.Validate where

import Control.Monad
import System.IO

import ICFPC2019.Utils
import ICFPC2019.Types
import ICFPC2019.StateUtils

--import Debug.Trace

type ValidationResult = Either (Int, ProblemState, Action) (Int, ProblemState)

validate :: Problem -> ProblemState -> [Action] -> ValidationResult
validate prob state0 = foldM step (1, state0)
  where
    step :: (Int, ProblemState) -> Action -> ValidationResult
    step (curIdx, state) act =
      case changeState prob state act of
        Just state' -> Right (curIdx + 1, state')
        Nothing     -> Left (curIdx, state, act)

printPath :: Problem -> ProblemState -> [Action] -> IO ()
printPath problem state0 actions = foldM_ printOne (1 :: Int, state0) actions
  where printOne (step, state) action = do
          state' <- case changeState problem state action of
                     Just state' -> return state'
                     Nothing -> fail "Failed to validate"
          hPutStrLn stderr ""
          hPutStrLn stderr $ "Step: " ++ show step
          hPutStrLn stderr $ "Action: " ++ show action
          hPutStrLn stderr $ "New state: " ++ show state'
          return (step + 1, state')
