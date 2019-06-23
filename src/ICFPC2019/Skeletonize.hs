module ICFPC2019.Skeletonize
  ( Cluster(..)
  , getCoreNodes
  , convertSkeleton
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.ST
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad
import System.IO
import System.Exit
import qualified System.Process as P
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as R
import Linear.V2

import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.IO

import Debug.Trace

data Cluster = Cluster { clusterNeighbours :: !(Set Int)
                       , clusterNodes :: !(Set I2)
                       }
             deriving (Show, Eq)


newtype SkeletonException = SkeletonException String
    deriving Show

instance Exception SkeletonException

getCoreNodes :: MapArray -> IO [I2]
getCoreNodes cells = do
  let processInfo = (P.proc "python/skeletonize.py" []) { P.std_in = P.CreatePipe
                                                        , P.std_out = P.CreatePipe
                                                        }
  P.withCreateProcess processInfo $ \(Just pstdin) (Just pstdout) _ ph -> do
    BB.hPutBuilder pstdin $ buildMapArray cells
    hClose pstdin
    tid <- myThreadId
    _ <- forkIO $ do
      ec <- P.waitForProcess ph
      unless (ec == ExitSuccess) $ throwTo tid $ SkeletonException $ show ec
    input <- BL.hGetContents pstdout
    case parse npArray input of
      Done _ coreNodes -> do
        let Z :. ySize :. _xSize = R.extent coreNodes

            getNode y = V2 nodeX nodeY
              where nodeX = coreNodes R.! (Z :. y :. 1)
                    nodeY = coreNodes R.! (Z :. y :. 0)
              
        return $ map getNode [0..ySize - 1]
      Fail _ ctx e -> fail ("Failed to parse in " ++ show ctx ++ ": " ++ e)

neighbours :: MapArray -> I2 -> [I2]
neighbours cells p =
  [ np
  | dp <- steps
  , let np@(V2 nx ny) = p + dp
  , nx >= 0 && nx < xSize
  , ny >= 0 && ny < ySize
  , cells R.! np
  ]
  where V2 xSize ySize = R.extent cells
        steps = [ V2 1    0
                , V2 0    1
                , V2 (-1) 0
                , V2 0    (-1)
                ]

type ClusterMap = Map Int Cluster

convertSkeleton ::  MapArray -> [I2] -> ClusterMap
convertSkeleton cells coreNodes = runST $ do
  let size = R.extent cells
  nodes <- VUM.replicate (R.size size) (-1 :: Int, maxBound :: Int)

  let fillNodes node [] = return ()
      fillNodes node ((p, len) : queue) = do
        (oldNode, oldLen) <- VUM.read nodes (R.toIndex size p)
        if len < oldLen
          then do
            VUM.write nodes (R.toIndex size p) (node, len)
            let newQueue = map (, len + 1) $ neighbours cells p
            fillNodes node (queue ++ newQueue)
          else fillNodes node queue

  zipWithM_ (\i p -> fillNodes i [(p, 0)]) [0..] coreNodes

  finalNodes <- VU.unsafeFreeze nodes
  visited <- VUM.replicate (R.size size) False

  let findNeighbours node cluster@(Cluster {..}) p = do
        let (currNode, _) = finalNodes VU.! R.toIndex size p
        if node /= currNode
          then return $ cluster { clusterNeighbours = S.insert currNode clusterNeighbours }
          else do
            isVisited <- VUM.read visited (R.toIndex size p)
            if isVisited
              then return cluster
              else do
                VUM.write visited (R.toIndex size p) True
                let cluster' = cluster { clusterNodes = S.insert p clusterNodes }
                foldM (findNeighbours node) cluster' $ neighbours cells p

  let emptyCluster =
        Cluster { clusterNeighbours = S.empty
                , clusterNodes = S.empty
                }
  fmap M.fromList $ zipWithM (\i p -> (i, ) <$> findNeighbours i emptyCluster p) [0..] coreNodes
  
{-
rectanglePaths :: I2 -> I2 -> [[I2]]
rectanglePaths a@(V2 x y) b
  | a == b = [[]]
  | otherwise = dxOpts ++ dyOpts
  where V2 dx dy = signum (b - a)
        dxOpts
          | dx == 0 = []
          | otherwise =
              let na = V2 (x + dx) y
              in map (na :) $ rectanglePaths na b
        dyOpts
          | dy == 0 = []
          | otherwise =
              let na = V2 x (y + dy)
              in map (na :) $ rectanglePaths na b

convertSkeleton :: MapArray -> MapArray -> Map I2 Cluster
convertSkeleton cells skel = runST $ do
  visitedSkel <- VUM.thaw skel
  nodes <- VUM.replicate (R.size $ R.extent cells) 0

  let findBackbone backbone p = do
        hasSkel <- VUM.read visitedSkel (R.toIndex size p)
        if not hasSkel
          then return backbone
          else do
            VUM.write visitedSkel (R.toIndex size p) False
            let neighbours = [ np
                             , dx <- [-1..1]
                             , dy <- [-1..1]
                             , not (dx == 0 && dy == 0)
                             , let np = p + V2 dx dy
                             , any (\(step:steps) -> null steps || cells R.! step) $ rectanglePaths p np
                             ]  -- 
            foldM findBackbone backbone neighbours

      fillNodes idx clusterPoints [] = return clusterPoints
      fillNodes idx clusterPoints (p : queue) = do
        backbone <- findCenter p S.empty
        if S.null center
          then fillNodes idx clusters queue
          else do
            currentId <- VM.read nodes (R.toIndex size p)
        if currentId /= 0
          then fillNodes idx clusters queue
          else do
            VM.write nodes (R.toIndex size p) idx
            cluster <- fillCluster idx (S.singleton p) p
            
-}
