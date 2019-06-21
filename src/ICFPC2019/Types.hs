module ICFPC2019.Types where

import Data.Set (Set)
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector (V)
import Linear.V2
  
import ICFPC2019.Utils

type I2 = V2 Int

data Cell = Obstacle
          | Free { cellWrapped :: !Bool
                 , cellObjects :: !(Set Booster)
                 }
          deriving (Show, Eq)

data Booster = Extension
             | FastWheels
             | Drill
             | Mysterious
             deriving (Show, Eq)

data Robot = Robot { robotPosition :: I2
                   , robotManipulators :: Set I2
                   }
             deriving (Show, Eq)

data Problem = Problem { problemMap :: Array V I2 Cell
                       , problemRobot :: Robot
                       , problemOffset :: I2
                       }
             deriving (Show, Eq)
