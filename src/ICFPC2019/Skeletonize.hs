module ICFPC2019.Skeletonize
  ( Cluster(..)
  , getRawClusters
  ) where

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

data Cluster = Cluster { neighbours :: !(Set Int)
                       , points :: !(Set I2)
                       }
             deriving (Show, Eq)

getRawClusters :: MapArray -> IO (Map Int (Set I2))
getRawClusters cells = do
  let processInfo = (P.proc "python/skeletonize.py" []) { P.std_in = P.CreatePipe
                                                        , P.std_out = P.CreatePipe
                                                        }
  P.withCreateProcess processInfo $ \(Just pstdin) (Just pstdout) _ ph -> do
    BB.hPutBuilder pstdin $ buildMapArray cells
    hClose pstdin
    ec <- P.waitForProcess ph
    unless (ec == ExitSuccess) $ fail "Failed to run Python"
    input <- BL.hGetContents pstdout
    case parse npArray input of
      Done _ nodeParams -> do
        let Z :. ySize :. xSize = R.extent nodeParams

            insertNode y clusters =
              M.insertWith S.union nodeCluster (S.singleton (V2 nodeX nodeY)) clusters
              where nodeX = nodeParams R.! (Z :. y :. 0)
                    nodeY = nodeParams R.! (Z :. y :. 1)
                    nodeCluster = nodeParams R.! (Z :. y :. 2)

            clusters = foldr insertNode M.empty [0..ySize - 1]

        return clusters
      Fail _ ctx e -> fail ("Failed to parse in " ++ show ctx ++ ": " ++ e)

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
        hasSkel <- VUM.read visitedSkel (R.index p)
        if not hasSkel
          then return backbone
          else do
            VUM.write visitedSkel (R.index p) False
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
            currentId <- VM.read nodes (R.index p)
        if currentId /= 0
          then fillNodes idx clusters queue
          else do
            VM.write nodes (R.index p) idx
            cluster <- fillCluster idx (S.singleton p) p
            
-}
