module ICFPC2019.Heuristics.Packager where

import Data.Set (Set)
import qualified Data.Set as S
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import ICFPC2019.Types
import ICFPC2019.Utils

uncycleTrace :: Problem -> ProblemState -> [(ProblemState, a)] -> [(ProblemState, a)]
uncycleTrace problem state trace =
  clearTrace trace $ mark M.empty trace 0
    where
      mark :: HashMap Robot (Int, Set I2) -> [(ProblemState, a)] -> Int -> [(Int, Int)]
      mark storage ((curState, _) : trace) curIdx =
        let
          curRobot = problemRobot curState
          curUnwrapped = problemUnwrapped curState
          storage' = M.insert curRobot (curIdx, curUnwrapped) storage
        in
        case M.lookup curRobot storage of
          Just (pos, unwrapped) -> if unwrapped == curUnwrapped
                                   then (pos, curIdx) : mark storage' trace (curIdx + 1)
                                   else mark storage' trace (curIdx + 1)
          _                     -> mark storage' trace (curIdx + 1)
      clearTrace :: [a] -> [(Int, Int)] -> [a]
      clearTrace trace marks = helper (zip [1..] trace) marks
        where
          helper trace [] = map snd trace
          helper []    _  = []
          helper trace@((curIdx, elem):otherTrace) marks@((curBegin, curEnd):otherMarks)
            | curIdx <= curBegin = elem : helper otherTrace marks
            | curIdx >  curEnd   = helper trace otherMarks
            | otherwise          = helper otherTrace marks
