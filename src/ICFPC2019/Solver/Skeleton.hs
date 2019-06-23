module ICFPC2019.Solver.Skeleton
  ( solve
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe

import DNIWEChan.Metric
import DNIWEChan.Graph
import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.Skeletonize
import ICFPC2019.Solver.Utils

import Debug.Trace

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

        assessCost (state', action) = ((localUnwrapped `S.intersection` problemUnwrapped state', state'), action, 1)

getMoveNeighbours :: Problem -> ProblemState -> [(ProblemState, Action, Int)]
getMoveNeighbours problem@Problem {..} state = map (\(state', action) -> (state', action, 1)) neighbours
  where neighbours = getNeighboursOfType problem state moves
        moves = [ MUp, MRight, MDown, MLeft
                ]

solve :: Problem -> ClusterMap -> ProblemState -> Maybe [(ProblemState, Action)]
solve problem clusters state0 = do
  path <- findClusterPath clusters state0
  traceM $ "found cluster path: " ++ show path
  findNext state0 $ map (clusters M.!) path

  where go :: ProblemState -> Set I2 -> [Cluster] -> Maybe [(ProblemState, Action)]
        go state unwrapped [] = error "solve.go: impossible"
        go state unwrapped (cluster:queue) = do
          traceM $ "filling at " ++ show (robotPosition $ problemRobot state) ++ ", points " ++ show unwrapped
          solution <- map (\((_, state'), action) -> (state', action)) <$> aStar (getClusterNeighbours problem cluster) (S.size . fst) (unwrapped, state) (S.null . fst)
          if null queue
            then return solution
            else do
              let state' =
                    case solution of
                      [] -> state
                      sol -> fst $ last sol
              next <- findNext state' queue
              return (solution ++ next)

        findNext :: ProblemState -> [Cluster] -> Maybe [(ProblemState, Action)]
        findNext state [] = return []
        findNext state queue@(cluster:others)
          | S.null unwrapped = findNext state others
          | otherwise = do
              traceM $ "moving from " ++ show (robotPosition $ problemRobot state) ++ " to " ++ show (clusterNodes cluster)
              solution <- aStar (getMoveNeighbours problem) (distanceToCluster cluster) state (isInCluster cluster)
              let state' =
                    case solution of
                      [] -> state
                      sol -> fst $ last sol
              next <- go state' unwrapped queue
              return (solution ++ next)
              
              where unwrapped = clusterNodes cluster `S.intersection` problemUnwrapped state

type PathState = (Set ClusterId, ClusterId)

findClusterPath :: ClusterMap -> ProblemState -> Maybe [ClusterId]
findClusterPath clusters state = fmap ((initialClusterId :) . map snd) $ aStar getMyNeighbours (S.size . fst) (initialUnvisited, initialClusterId) (S.null . fst)
  where initialUnvisited = S.delete initialClusterId $ M.keysSet clusters
        initialClusterId = fst $ head $ filter (\(_, cluster) -> robotPosition (problemRobot state) `S.member` clusterNodes cluster) $ M.toList clusters

        lookupNeighbours clusterId = map (\c -> (c, c)) $ S.toList $ clusterNeighbours $ clusters M.! clusterId

        getMyNeighbours :: PathState -> [(PathState, ClusterId, Int)]
        getMyNeighbours (unvisitedClusters, clusterId)
          | null visitMoves = moveoutMoves
          | otherwise = visitMoves
          
          where cluster =
                  --trace "" $
                  --trace ("id: " ++ show clusterId) $
                  --trace ("state: " ++ show unvisitedClusters) $
                  clusters M.! clusterId
                visitMoves = take 1 $ mapMaybe updateState $ S.toList $ clusterNeighbours cluster

                updateState nextId
                  | S.size unvisitedClusters' < S.size unvisitedClusters = Just ((unvisitedClusters', nextId), nextId, 1)
                  | otherwise = Nothing
                  where unvisitedClusters' = S.delete nextId unvisitedClusters

                closestPath = map fst $ fromJust $ let r = bfs lookupNeighbours clusterId (`S.member` unvisitedClusters) in r
                closestCluster = last closestPath
                moveoutMoves = [((S.delete closestCluster unvisitedClusters, closestCluster), closestCluster, length closestPath)]
