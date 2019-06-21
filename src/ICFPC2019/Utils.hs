module ICFPC2019.Utils where

import Data.Foldable
import Linear.V2
import Data.Array.Repa.Shape

instance Shape (V2 Int) where
  rank _ = 2
  zeroDim = 0
  unitDim = 1
  intersectDim = min
  addDim = (+)
  size = foldr1 (*)
  sizeIsValid (V2 x y) = x * y >= 0
  toIndex (V2 xSize ySize) (V2 x y) = y * xSize + x
  fromIndex (V2 xSize ySize) idx = V2 x y
    where (y, x) = idx `divMod` xSize
  inShapeRange (V2 x1 y1) (V2 x2 y2) (V2 x y) =
    x >= x1 && x <= x2 &&
    y >= y1 && y <= y2
  listOfShape = toList

  shapeOfList [x, y] = V2 x y
  shapeOfList _ = error "shapeOfList (V2 Int): invalid shape"

  deepSeq = seq
