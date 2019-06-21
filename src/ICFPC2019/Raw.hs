module ICFPC2019.Raw
  ( RectilinearPoly
  , RawProblem(..)
  , convertProblem
  ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Set as S
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as R
import Control.Lens
import Linear.V2

import ICFPC2019.Types
import ICFPC2019.Utils
 
type RectilinearPoly = [I2]

data RawProblem = RawProblem { rawMap :: RectilinearPoly
                             , rawPosition :: I2
                             , rawObstacles :: [RectilinearPoly]
                             , rawBoosters :: [(Booster, I2)]
                             }
                deriving (Show, Eq)

rectangle :: [a] -> [(a, a)]
rectangle points@(h : t@(_:_)) = zip points t ++ [(last t, h)]
rectangle _ = error "rectangle: not enough points"

linePoints :: (Eq a, Num a) => a -> a -> [a]
linePoints a b
  | a == b = [a]
  | otherwise = points a
  where step = signum (b - a)
        points p
          | p == b = [p]
          | otherwise = p : points (p + step)

data BorderCell = BEmpty
                | BVisited
                | BBorder
                deriving (Show, Eq)

neighbours :: (Ord a, Num a) => V2 a -> V2 a -> [V2 a]
neighbours (V2 xSize ySize) (V2 x y) = [ V2 nx ny
                                       | dx <- [-1, 0, 1]
                                       , dy <- [-1, 0, 1]
                                       , let nx = x + dx
                                       , let ny = y + dy
                                       , not (dx == 0 && dy == 0)
                                       , nx >= 0 && nx < xSize
                                       , ny >= 0 && ny < ySize
                                       ]

adjastents :: Num a => V2 a -> [V2 a]
adjastents (V2 x y) = [ V2 x       y
                      , V2 x       (y + 1)
                      , V2 (x + 1) (y + 1)
                      , V2 (x + 1) y
                      ]

convertProblem :: RawProblem -> Problem
convertProblem (RawProblem { .. }) =
  Problem { problemMap
          , problemRobot
          , problemOffset = offset
          }
  
  where minX = minimum (map (^. _x) rawMap) - 1
        maxX = maximum (map (^. _x) rawMap) - 1
        minY = minimum (map (^. _y) rawMap) + 1
        maxY = maximum (map (^. _y) rawMap) + 1

        offset = V2 minX minY
        mapSize = V2 (maxX - minX) (maxY - minY)
        bordersSize = V2 (maxX - minX + 1) (maxY - minY + 1)
        start@(V2 x0 y0) = rawPosition - offset

        problemRobot = Robot { robotPosition = start
                             , robotManipulators = S.fromList [ start
                                                              , V2 (x0 + 1) y0
                                                              , V2 (x0 + 1) (y0 + 1)
                                                              , V2 (x0 + 1) (y0 - 1)
                                                              ]
                             }

        problemMap :: R.Array R.V I2 Cell
        problemMap = runST $ do
          cells <- VM.replicate (R.size mapSize) Obstacle
          borders <- VM.replicate (R.size bordersSize) BEmpty
          let borderPoints = concatMap (concatMap (uncurry linePoints) . rectangle) (rawMap : rawObstacles)
          forM_ borderPoints $ \p -> do
            VM.write borders (R.toIndex bordersSize (p - offset)) BBorder

          let fillBorders [] = return ()
              fillBorders (p : queue) = do
                val <- VM.read borders (R.toIndex bordersSize p)
                case val of
                  BEmpty -> do
                    VM.write borders (R.toIndex bordersSize p) BVisited
                    fillBorders (neighbours bordersSize p ++ queue)
                  _ -> fillBorders queue

          fillBorders $ adjastents start
          finalBorders <- V.unsafeFreeze borders

          let fillCells [] = return ()
              fillCells (p : queue) = do
                val <- VM.read cells (R.toIndex mapSize p)
                case val of
                  Obstacle | all (\bp -> (finalBorders V.! R.toIndex bordersSize bp) /= BEmpty) $ adjastents p -> do
                                 VM.write cells (R.toIndex mapSize p) $
                                   Free { cellWrapped = False
                                        , cellObjects = S.empty
                                        }
                                 fillCells (neighbours mapSize p ++ queue)
                  _ -> fillCells queue

          fillCells [start]

          forM_ rawBoosters $ \(booster, p) -> do
            let idx = R.toIndex mapSize (p - offset)
            state <- VM.read cells idx
            VM.write cells idx $ state { cellObjects = S.insert booster $ cellObjects state }

          finalCells <- V.unsafeFreeze cells
          return $ R.fromVector mapSize finalCells
