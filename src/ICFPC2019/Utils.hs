{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICFPC2019.Utils where

import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable
import Linear.V2
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector (V)
import qualified Data.Array.Repa.Repr.Vector as R
import Data.Array.Repa.Shape
import Data.Hashable (Hashable, hashWithSalt, hash)

type I2 = V2 Int

instance Shape I2 where
  rank _ = 2
  zeroDim = 0
  unitDim = 1
  intersectDim = min
  addDim = (+)
  size = foldr1 (*)
  sizeIsValid s = size s >= 0
  toIndex (V2 xSize ySize) (V2 x y) = y * xSize + x
  fromIndex (V2 xSize ySize) idx = V2 x y
    where (y, x) = idx `divMod` xSize
  inShapeRange (V2 x1 y1) (V2 x2 y2) (V2 x y) =
    x >= x1 && x <= x2 &&
    y >= y1 && y <= y2
  listOfShape = toList

  shapeOfList [x, y] = V2 x y
  shapeOfList _ = error "shapeOfList (I2): invalid shape"

  deepSeq = seq

wrapVectorRepa :: Shape sh => (V.Vector a -> V.Vector b) -> R.Array V sh a -> R.Array V sh b
wrapVectorRepa f arr = R.fromVector (R.extent arr) $ f $ R.toVector arr

wrapVectorRepaM :: (Applicative f, Shape sh) => (V.Vector a -> f (V.Vector b)) -> R.Array V sh a -> f (R.Array V sh b)
wrapVectorRepaM f arr = fmap (R.fromVector (R.extent arr)) $ f $ R.toVector arr

instance Shape sh => Functor (R.Array V sh) where
  fmap f = wrapVectorRepa (fmap f)

instance Shape sh => Foldable (R.Array V sh) where
  foldMap f arr = foldMap f $ R.toVector arr
  foldr f acc arr = foldr f acc $ R.toVector arr

instance Shape sh => Traversable (R.Array V sh) where
  traverse f = wrapVectorRepaM (traverse f)

indexArray :: (R.Source v a) => R.Array v I2 a -> S.Set I2
indexArray arr = S.fromList $ map (R.fromIndex curShape) [0 .. (R.size curShape) - 1]
  where
    curShape = R.extent arr

instance (Hashable a) => Hashable (Set a) where
  hashWithSalt salt set = hashWithSalt salt $ S.toList set
