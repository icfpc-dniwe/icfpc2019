module ICFPC2019.Types where

import Data.Set (Set)
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector (V)
  
import ICFPC2019.Utils
import ICFPC2019.Visualize

data Cell = Obstacle
          | Free { cellWrapped :: !Bool
                 , cellObjects :: !(Set Booster)
                 }
          deriving (Show, Eq)

instance CharShow Cell where
  charShow Obstacle = '#'
  charShow Free { .. }
    | cellWrapped = ','
    | otherwise = '.'

data Booster = Extension
             | FastWheels
             | Drill
             | Mysterious
             deriving (Show, Eq, Ord)

data Robot = Robot { robotPosition :: I2
                   , robotManipulators :: Set I2
                   }
             deriving (Show, Eq)

data Problem = Problem { problemMap :: Array V I2 Cell
                       , problemRobot :: Robot
                       , problemOffset :: I2
                       }
             deriving (Show, Eq)

data Action = MUp
            | MDown
            | MLeft
            | MRight
            | MNothing
            | MTurnRight
            | MTurnLeft
            | MAttachManipulator I2
            | MAttachWheels
            | MAttachDrill
  deriving (Show, Eq, Ord, Bounded, Enum)
