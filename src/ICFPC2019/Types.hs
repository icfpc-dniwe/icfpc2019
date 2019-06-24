module ICFPC2019.Types where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import Data.HashMap.Strict (HashMap)
import Data.Array.Repa
import Data.Array.Repa.Repr.Unboxed (U)
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

-- True - empty
-- False - obstacle
type MapArray = Array U I2 Bool

data Orientation = E | N | W | S
                 deriving (Show, Eq, Ord, Generic)

data Rotation = L | R
              deriving (Show, Eq, Ord)

data Robot = Robot { robotPosition :: !I2
                   , robotManipulators :: !(Set I2)
                   , robotBoosters :: !(HashMap Booster Int)
                   , robotBeacons :: !(Set I2)
                   , robotDrillLeft :: !Int
                   , robotWheelsLeft :: !Int
                   }
             deriving (Show, Eq, Ord, Generic)

data Problem = Problem { problemMap :: !MapArray
                       , problemOffset :: !I2
                       }
             deriving (Show, Eq)

data ProblemState = ProblemState { problemBoosters :: !(HashMap I2 (Set Booster))
                                 , problemUnwrapped :: !(Set I2)
                                 , problemRobot :: !Robot
                                 , problemDrilled :: !(Set I2)
                                 }
                  deriving (Show, Ord, Eq, Generic)

instance Hashable Orientation where
instance Hashable ProblemState where
instance Hashable Booster where
instance Hashable Robot where

type ActionPriority = Map Action Int

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
            | MTeleport !I2
            deriving (Show, Eq, Ord)
