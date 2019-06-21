module ICFPC2019.RobotUtils (
    speed
    ,drillEnabled
    ,hasUnspentBeacons
    ,applyAction
    ) where

import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2 (V2(..))

import ICFPC2019.Types
import ICFPC2019.Utils

speed :: Robot -> Int
speed (Robot {..}) = if robotWheelsLeft > 0 then 2
                                            else 1

drillEnabled :: Robot -> Bool
drillEnabled (Robot {..}) = robotDrillLeft > 0

hasUnspentBeacons :: Robot -> Bool
hasUnspentBeacons (Robot {..}) = robotBeaconsLeft > 0

-- move' :: crd -> action --> speed -> new crd
move' :: I2 -> Action -> Int -> I2
move' (V2 x y) MUp s = V2 x (y+s)
move' (V2 x y) MDown s = V2 x (y-s)
move' (V2 x y) MRight s = V2 (x-s) y
move' (V2 x y) MLeft s = V2 (x+s) y

rot' :: I2 -> Action -> I2
rot' (V2 x y) MTurnLeft = V2 (-y) x
rot' (V2 x y) MTurnRight = V2 y (-x)

decrementBoosters' :: Robot -> Robot
decrementBoosters' r = r {
    robotWheelsLeft = max 0 $ robotWheelsLeft r - 1,
    robotDrillLeft = max 0 $ robotDrillLeft r - 1
}

applyMoveAction' :: Robot -> Action -> Robot
applyMoveAction' r action = decrementBoosters' $ r {
    robotPosition = move' (robotPosition r) action $ speed r
}

applyRotAction' :: Robot -> Action -> Robot
applyRotAction' r action = decrementBoosters' $ r {
    robotManipulators = S.fromList $ map (\m -> rot' m action) $ S.toList $ robotManipulators r
}

applyAction :: Robot -> Action -> Robot
applyAction r MNothing = r
applyAction r MUp = applyMoveAction' r MUp
applyAction r MDown = applyMoveAction' r MDown
applyAction r MLeft = applyMoveAction' r MLeft
applyAction r MRight = applyMoveAction' r MRight
applyAction r MTurnLeft = applyRotAction' r MTurnLeft
applyAction r MTurnRight = applyRotAction' r MTurnRight
applyAction r (MAttachManipulator m) = decrementBoosters' $ r {
    robotManipulators = S.insert m $ robotManipulators r
}
applyAction r MAttachWheels = decrementBoosters' $ r {
    robotWheelsLeft = max 50 $ robotWheelsLeft r
}
applyAction r MAttachDrill = decrementBoosters' $ r {
    robotDrillLeft = max 30 $ robotDrillLeft r
}
applyAction r MPlaceBeacon = decrementBoosters' $ r {
    robotBeacons = S.insert (robotPosition r) $ robotBeacons r,
    robotBeaconsLeft = max 0 $ robotBeaconsLeft r - 1
}
applyAction r (MTeleport b) = decrementBoosters' $ r {
    robotPosition = b
}
