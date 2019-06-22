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

{-# INLINE bfs #-}
bfs :: forall i tag score. (Hashable i, Eq i, Ord score, Num score) => (i -> [(i, tag, score)]) -> i -> (i -> Bool) -> Maybe [(i, tag)]
bfs getNeighbours start finishCheck = go [start] (S.singleton start) M.empty
  where
    go :: [i] -> HashSet i -> HashMap i (i, tag) -> Maybe [(i, tag)]
    go [] discovered parents = Nothing
    go (curNode : seq) discovered parents =
      if finishCheck curNode
      then Just $ reverse $ traverse curNode
      else go (neighboursNode ++ seq) (S.fromList neighboursNode `S.union` discovered) parents'
      where
        neighbours = map (\(node, tag, _) -> (node, tag)) . filter (\(node, _, _) -> not $ S.member node discovered) $ getNeighbours curNode
        neighboursNode = map fst neighbours
        parents' = foldr (\(node, tag) par -> M.insert node (curNode, tag) par) parents neighbours
        traverse :: i -> [(i, tag)]
        traverse node = case M.lookup node parents of
                          Just (parentNode, tag) -> (node, tag) : traverse parentNode
                          _                      -> []

solve :: Problem -> ProblemState -> Maybe [(ProblemState, Action)]
solve problem state = bfs (getNeighbours problem) state checker
  where
    checker curState = DS.null $ problemUnwrapped curState
