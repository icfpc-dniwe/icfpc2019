module ICFPC2019.Visualize
  ( CharShow(..)
  , showPlane
  ) where

import qualified Data.Array.Repa as R
import Linear.V2

import ICFPC2019.Utils

class CharShow a where
  charShow :: a -> Char

instance CharShow Bool where
  charShow True =  '.'
  charShow False = '#'

instance CharShow Int where
  charShow i
    | i >= 0 && i <= 9 = head $ show i
    | otherwise = error "Too big i"

showPlane :: (CharShow e, R.Source r e) => R.Array r I2 e -> String
showPlane arr = concatMap showLine [ySize - 1,ySize - 2..0]
  where V2 xSize ySize = R.extent arr

        showLine y = map (\x -> charShow $ arr R.! V2 x y) [0..xSize - 1] ++ "\n"
