module ICFPC2019.Types where

import Data.Set (Set)
import qualified Data.Set as S
import Data.HashMap.Strict (HashMap)
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector (V)
import Data.Hashable (Hashable, hashWithSalt, hash)
import GHC.Generics (Generic)

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
             deriving (Show, Eq, Ord, Generic)

type MapArray a = Array V I2 a

data Orientation = E | N | W | S
                 deriving (Show, Eq, Ord)

data Robot = Robot { robotPosition :: !I2
                   , robotManipulators :: !(Set I2)
                   , robotUnspentManips :: !Int
                   , robotBeacons :: !(Set I2)
                   , robotUnspentBeacons :: !Int
                   , robotDrillLeft :: !Int
                   , robotUnspentDrills :: !Int
                   , robotWheelsLeft :: !Int
                   , robotUnspentWheels :: !Int
                   }
             deriving (Show, Eq, Ord, Generic)

data Problem = Problem { problemMap :: !(MapArray Cell)
                       , problemOffset :: !I2
                       }
             deriving (Show, Eq)

data ProblemState = ProblemState { problemBoosters :: !(HashMap I2 (Set Booster))
                                 , problemUnwrapped :: !(Set I2)
                                 , problemRobot :: !Robot
                                 }
                  deriving (Show, Ord, Eq, Generic)

instance Hashable ProblemState where
instance Hashable Booster where
instance Hashable Robot where

instance (Hashable a) => Hashable (Set a) where
  hashWithSalt salt set = hashWithSalt salt $ S.toList set

data Action = MUp
            | MDown
            | MLeft
            | MRight
            | MNothing
            | MTurnRight
            | MTurnLeft
            | MPickUpManipulator
            | MAttachManipulator !I2
            | MPickUpWheels
            | MAttachWheels
            | MPickUpDrill
            | MAttachDrill
            | MPickUpBeacon
            | MPlaceBeacon
            | MTeleport I2
            deriving (Show, Eq, Ord)
