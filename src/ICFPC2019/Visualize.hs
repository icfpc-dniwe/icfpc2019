module ICFPC2019.Visualize
  ( CharShow(..)
  , showPlane
  ) where

import qualified Data.Array.Repa as R
import Linear.V2

import ICFPC2019.Utils

class CharShow a where
  charShow :: a -> Char

showPlane :: (CharShow e, R.Source r e) => R.Array r I2 e -> String
showPlane arr = concatMap showLine [0..ySize - 1]
  where V2 xSize ySize = R.extent arr

        showLine y = map (\x -> charShow $ arr R.! V2 x y) [0..xSize - 1]
