module ICFPC2019.Types where

import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector (V)

import ICFPC2019.Utils
import ICFPC2019.Visualize

data Cell = Obstacle
          | Free
          deriving (Show, Eq)

instance CharShow Cell where
  charShow Obstacle = '#'
  charShow Free = '.'

data Booster = Extension
             | FastWheels
             | Drill
             | Mysterious
             | Teleport
             deriving (Show, Eq, Ord)

data Robot = Robot { robotPosition :: !I2
                   , robotManipulators :: !(Set I2)
                   }
             deriving (Show, Eq)

type MapArray a = Array V I2 a

data Problem = Problem { problemMap :: !(MapArray Cell)
                       , problemOffset :: !I2

                       , problemBoosters :: !(Map I2 (Set Booster))
                       , problemUnwrapped :: !(Set I2)
                       , problemRobot :: !Robot
                       }
             deriving (Show, Eq)

data Action = MUp
            | MDown
            | MLeft
            | MRight
            | MNothing
            | MTurnRight
            | MTurnLeft
            | MAttachManipulator !I2
            | MAttachWheels
            | MAttachDrill
            | MPlaceBeacon
            | MTeleport I2
            deriving (Show, Eq, Ord)
