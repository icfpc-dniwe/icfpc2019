module ICFPC2019.Solver.BFS where

--import Data.Sequence (Sequence)
--import qualified Data.Sequence as Q
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import qualified Data.Set as DS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)

import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.Solver.Utils

bfs :: forall i tag score. (Hashable i, Eq i, Ord score, Num score) => (i -> [(i, tag, score)]) -> i -> (i -> Bool) -> Maybe [(i, tag)]
bfs getNeighbours start finishCheck = go [start] (M.singleton start Nothing)
  where
    go :: [i] -> HashMap i (Maybe (i, tag)) -> Maybe [(i, tag)]
    go [] parents = Nothing
    go (curNode : seq) parents =
      if finishCheck curNode
      then Just $ reverse $ traverse curNode
      else go (seq ++ neighboursNode) parents'
      where
        neighbours = map (\(node, tag, _) -> (node, tag)) $ filter (\(node, _, _) -> not $ M.member node parents) $ getNeighbours curNode
        neighboursNode = map fst neighbours
        parents' = foldr (\(node, tag) -> M.insert node (Just (curNode, tag))) parents neighbours
        traverse :: i -> [(i, tag)]
        traverse node = case M.lookup node parents of
                          Just (Just (parentNode, tag)) -> (node, tag) : traverse parentNode
                          Just Nothing                  -> []
                          Nothing                       -> error "traverse: no parent"

solve :: Problem -> ProblemState -> Maybe [(ProblemState, Action)]
solve problem state = bfs (getNeighbours problem) state checker
  where
    checker curState = DS.null $ problemUnwrapped curState
