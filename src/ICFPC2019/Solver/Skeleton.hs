module ICFPC2019.Solver.Skeleton
  ( solve
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import DNIWEChan.Metric
import DNIWEChan.Graph
import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.Skeletonize
import ICFPC2019.Solver.Utils

type ClusterState = (Set I2, ProblemState)

isInCluster :: Cluster -> ProblemState -> Bool
isInCluster cluster state = robotPosition (problemRobot state) `S.member` clusterNodes cluster

distanceToCluster :: Cluster -> ProblemState -> Int
distanceToCluster cluster state = minimum $ map (robotPosition (problemRobot state) `mlenDistance`) $ S.toList $ clusterNodes cluster

getClusterNeighbours :: Problem -> Cluster -> ClusterState -> [(ClusterState, Action, Int)]
getClusterNeighbours problem@Problem {..} cluster (localUnwrapped, state) = map assessCost $ filter (isInCluster cluster . fst) neighbours
  where neighbours = getNeighboursOfType problem state moves
        moves = [ MUp, MRight, MDown, MLeft
                , MTurnLeft, MTurnRight
                ]

        assessCost (state', action) = ((localUnwrapped `S.union` problemUnwrapped state', state'), action, 1)

getMoveNeighbours :: Problem -> ProblemState -> [(ProblemState, Action, Int)]
getMoveNeighbours problem@Problem {..} state = map (\(state', action) -> (state', action, 1)) neighbours
  where neighbours = getNeighboursOfType problem state moves
        moves = [ MUp, MRight, MDown, MLeft
                ]

solve :: Problem -> ClusterMap -> ProblemState -> Maybe [(ProblemState, Action)]
solve problem clusters state0 = do
  path <- findClusterPath clusters state0
  go state0 $ map (clusters M.!) path

  where go :: ProblemState -> [Cluster] -> Maybe [(ProblemState, Action)]
        go state [] = return []
        go state (cluster:queue) = do
          let unwrapped = clusterNodes cluster `S.union` problemUnwrapped state
          solution <- map (\((_, state'), action) -> (state', action)) <$> aStar (getClusterNeighbours problem cluster) (S.size . fst) (unwrapped, state) (S.null . fst)
          case queue of
            [] -> return solution
            next:_ -> do
              let state' =
                    case solution of
                      [] -> state
                      sol -> fst $ last sol
              solutionMove <- aStar (getMoveNeighbours problem) (distanceToCluster cluster) state' (isInCluster next)
              solutionEnd <- go (fst $ last solutionMove) queue
              return (solution ++ solutionMove ++ solutionEnd)

type PathState = (Set ClusterId, ClusterId)

findClusterPath :: ClusterMap -> ProblemState -> Maybe [ClusterId]
findClusterPath clusters state = fmap ((initialClusterId :) . map snd) $ aStar getMyNeighbours (S.size . fst) (M.keysSet clusters, initialClusterId) (S.null . fst)
  where initialClusterId = fst $ head $ filter (\(_, cluster) -> robotPosition (problemRobot state) `S.member` clusterNodes cluster) $ M.toList clusters

        getMyNeighbours :: PathState -> [(PathState, ClusterId, Int)]
        getMyNeighbours (usedClusters, clusterId) = map updateState $ S.toList clusterNeighbours
          where Cluster {..} = clusters M.! clusterId
                updateState nextId = ((S.delete nextId usedClusters, nextId), nextId, 1)
