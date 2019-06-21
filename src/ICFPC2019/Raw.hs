module ICFPC2019.Raw
  ( RectilinearPoly
  , RawProblem(..)
  , convertProblem
  ) where

import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
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

orderPair :: Ord a => a -> a -> (a, a)
orderPair a b
  | a < b = (a, b)
  | otherwise = (b, a)

lineVecs :: forall a. (Ord a, Num a) => a -> a -> Set (a, a)
lineVecs a b = vecs a S.empty
  where step = signum (b - a)
        vecs p vs
          | p == b = vs
          | otherwise = vecs np $ S.insert (orderPair p np) vs
          where np = p + step

neighbours :: (Ord a, Num a) => Set (V2 a, V2 a) -> V2 a -> V2 a -> [V2 a]
neighbours borders (V2 xSize ySize) p =
  [ np
  | dp <- steps
  , let np@(V2 nx ny) = p + dp
  , nx >= 0 && nx < xSize
  , ny >= 0 && ny < ySize
  , not (stepBorder dp p `S.member` borders)
  ]
  where steps = [ V2 1    0
                , V2 0    1
                , V2 (-1) 0
                , V2 0    (-1)
                ]

        stepBorder (V2 1    0   ) (V2 x y) = (V2 (x + 1) y       , V2 (x + 1) (y + 1))
        stepBorder (V2 0    1   ) (V2 x y) = (V2 x       (y + 1) , V2 (x + 1) (y + 1))
        stepBorder (V2 (-1) 0   ) (V2 x y) = (V2 x       y       , V2 x       (y + 1))
        stepBorder (V2 0    (-1)) (V2 x y) = (V2 x       y       , V2 (x + 1) y)
        stepBorder _ _ = error "stepBorder: invalid step"

convertProblem :: RawProblem -> Problem
convertProblem (RawProblem { .. }) =
  Problem { problemMap
          , problemUnwrapped
          , problemRobot
          , problemBoosters
          , problemOffset = offset
          }
  
  where minX = minimum (map (^. _x) rawMap)
        maxX = maximum (map (^. _x) rawMap)
        minP = V2 minX minY
        minY = minimum (map (^. _y) rawMap)
        maxY = maximum (map (^. _y) rawMap)
        maxP = V2 maxX maxY

        offset = minP
        mapSize = maxP - minP
        start = rawPosition - offset

        manipulators = [ V2 0 0
                       , V2 1 0
                       , V2 1 1
                       , V2 1 (-1)
                       ]

        problemRobot = Robot { robotPosition = start
                             , robotManipulators = S.fromList manipulators
                             , robotBeacons = S.empty
                             }

        problemBoosters = M.fromListWith S.union $ map (\(booster, p) -> (p, S.singleton booster)) rawBoosters

        (problemUnwrapped, problemMap) = runST $ do
          let borderPoints = foldr1 S.union $ map (foldr1 S.union . map (uncurry lineVecs) . rectangle) (rawMap : rawObstacles)

          cells <- VM.replicate (R.size mapSize) Obstacle

          let fillCells [] unwrapped = return unwrapped
              fillCells (p : queue) unwrapped = do
                val <- VM.read cells (R.toIndex mapSize p)
                case val of
                  Obstacle -> do
                    VM.write cells (R.toIndex mapSize p) Free
                    let newQueue = neighbours borderPoints mapSize p
                        newUnwrapped = S.insert p unwrapped
                    fillCells (newQueue ++ queue) newUnwrapped
                  _ -> fillCells queue unwrapped

          unwrapped <- fillCells [start] S.empty
          
          let finalUnwrapped = unwrapped S.\\ S.fromList (map (+ start) manipulators)

          finalCells <- V.unsafeFreeze cells
          return (finalUnwrapped, R.fromVector mapSize finalCells)
