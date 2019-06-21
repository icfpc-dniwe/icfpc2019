module ICFPC2019.Types where

import Data.Set (Set)
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector (V)
import Linear.V2

type I2 = V2 Int

data Cell = Obstable
          | Free { cellWrapped :: Bool
                 , cellObjects :: Set Booster
                 }
          deriving (Show, Eq)

data Booster = Extension
             | FastWheels
             | Drill
             | Mysterious
             deriving (Show, Eq)

data Robot = Robot { robotPosition :: DIM2
                   , robotManipulators :: Set DIM2
                   }
             deriving (Show, Eq)

data Problem = Problem { problemMap :: Array V DIM2 Cell
                       , problemRobot :: Robot
                       }
             deriving (Show, Eq)
