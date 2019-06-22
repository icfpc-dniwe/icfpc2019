module ICFPC2019.RobotUtils
  ( move
  , rot
  , speed
  , drillEnabled
  , hasUnspentBeacons
  , checkBoundaries
  , checkObstacles
  , applyAction
  , validateRobot
  , applyOrientation
  , rotateLeft
  , rotateRight
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M
import qualified Data.Array.Repa as R
import Linear.V2 (V2(..))
import Data.Maybe

import ICFPC2019.Types
import ICFPC2019.Utils

speed :: Robot -> Int
speed (Robot {..}) = if robotWheelsLeft > 0 then 2
                                            else 1

drillEnabled :: Robot -> Bool
drillEnabled (Robot {..}) = robotDrillLeft > 0

hasUnspentBeacons :: Robot -> Bool
hasUnspentBeacons (Robot {..}) = robotUnspentBeacons > 0

hasUnspentDrills :: Robot -> Bool
hasUnspentDrills (Robot {..}) = robotUnspentDrills > 0

hasUnspentWheels :: Robot -> Bool
hasUnspentWheels (Robot {..}) = robotUnspentWheels > 0

hasUnspentManips :: Robot -> Bool
hasUnspentManips (Robot {..}) = robotUnspentManips > 0

-- move :: crd -> action --> speed -> new crd
move :: I2 -> Action -> Int -> I2
move (V2 x y) MUp s = V2 x (y+s)
move (V2 x y) MDown s = V2 x (y-s)
move (V2 x y) MRight s = V2 (x+s) y
move (V2 x y) MLeft s = V2 (x-s) y

rot :: I2 -> Action -> I2
rot (V2 x y) MTurnLeft = V2 (-y) x
rot (V2 x y) MTurnRight = V2 y (-x)

checkBoundaries :: MapArray a -> I2 -> Bool
checkBoundaries gameMap = R.inShapeRange (V2 0 0) (R.extent gameMap - 1)

checkObstacles :: MapArray Cell -> I2 -> Bool
checkObstacles gameMap pos = (gameMap R.! pos) /= Obstacle

decrementBoosters :: Robot -> Robot
decrementBoosters r = r {
    robotWheelsLeft = max 0 $ robotWheelsLeft r - 1,
    robotDrillLeft = max 0 $ robotDrillLeft r - 1
}

applyMoveAction :: Robot -> Action -> Int -> Robot
applyMoveAction r action s = decrementBoosters $ r {
    robotPosition = move (robotPosition r) action s
}

applyRotAction :: Robot -> Action -> Robot
applyRotAction r action = decrementBoosters $ r {
    robotManipulators = S.fromList $ map (\m -> rot m action) $ S.toList $ robotManipulators r
}

applyAction' :: Robot -> Action -> Robot
applyAction' r MNothing = decrementBoosters r
--applyAction' r MUp = applyMoveAction r MUp
--applyAction' r MDown = applyMoveAction r MDown
--applyAction' r MLeft = applyMoveAction r MLeft
--applyAction' r MRight = applyMoveAction r MRight
applyAction' r MTurnLeft = applyRotAction r MTurnLeft
applyAction' r MTurnRight = applyRotAction r MTurnRight
applyAction' r MPickUpManipulator = r {
    robotUnspentManips = 1 + robotUnspentManips r
}
applyAction' r (MAttachManipulator m) = decrementBoosters $ r {
    robotManipulators = S.insert m $ robotManipulators r
}
applyAction' r MPickUpWheels = r {
    robotUnspentWheels = 1 + robotUnspentWheels r
}
applyAction' r MAttachWheels = decrementBoosters $ r {
    robotWheelsLeft = max 50 $ robotWheelsLeft r
}
applyAction' r MPickUpDrill = r {
    robotUnspentDrills = 1 + robotUnspentDrills r
}
applyAction' r MAttachDrill = decrementBoosters $ r {
    robotDrillLeft = max 30 $ robotDrillLeft r
}
applyAction' r MPickUpBeacon = r {
    robotUnspentBeacons = 1 + robotUnspentBeacons r
}
applyAction' r MPlaceBeacon = decrementBoosters $ r {
    robotBeacons = S.insert (robotPosition r) $ robotBeacons r,
    robotUnspentBeacons = max 0 $ robotUnspentBeacons r - 1
}
applyAction' r (MTeleport b) = decrementBoosters $ r {
    robotPosition = b
}

findValid :: [Maybe a] -> Maybe a
findValid [] = Nothing
findValid ((Just x):t) = Just x
findValid (Nothing:t) = findValid t

validateRobot :: MapArray Cell -> ProblemState -> Robot -> Maybe Robot
validateRobot map_ state r = 
    let rpos = robotPosition r
        drill = drillEnabled r
        valid = all id [
                checkBoundaries map_ rpos,
                not drill && (checkObstacles map_ rpos)
            ]
    in
        if valid then Just r
                 else Nothing

applyValidMoveAction :: MapArray Cell -> ProblemState -> Action -> Robot -> Maybe Robot
applyValidMoveAction map_ state action r =
    let possibleRobots = map (applyMoveAction r action) [1..(speed r)]
        validRobot = findValid $ (validateRobot map_ state) <$> possibleRobots
    in
        case validRobot of
            Just x -> Just (decrementBoosters x)
            Nothing -> Nothing 

boosterAvailable :: I2 -> ProblemState -> Booster -> Bool
boosterAvailable pos state bst = S.member bst $ (problemBoosters state) M.! pos

applyAction :: Robot -> MapArray Cell -> ProblemState -> Action -> Maybe Robot
applyAction r map_ state MNothing = Just (applyAction' r MNothing)
applyAction r map_ state MUp = applyValidMoveAction map_ state MUp r
applyAction r map_ state MDown = applyValidMoveAction map_ state MDown r
applyAction r map_ state MLeft = applyValidMoveAction map_ state MLeft r
applyAction r map_ state MRight = applyValidMoveAction map_ state MRight r
applyAction r map_ state MTurnRight = Just (applyAction' r MTurnRight)
applyAction r map_ state MTurnLeft = Just (applyAction' r MTurnLeft)

applyAction r map_ state MPickUpManipulator = 
    let avail = boosterAvailable (robotPosition r) state Extension
    in
        if avail then Just (applyAction' r MPickUpManipulator)
                 else Nothing
applyAction r map_ state (MAttachManipulator m) = 
    let avail = hasUnspentManips r
    in
        if avail then Just (applyAction' r (MAttachManipulator m))
                 else Nothing

applyAction r map_ state MPickUpWheels = 
    let avail = boosterAvailable (robotPosition r) state FastWheels
    in
        if avail then Just (applyAction' r MPickUpWheels)
                 else Nothing
applyAction r map_ state MAttachWheels = 
    let avail = hasUnspentWheels r
    in
        if avail then Just (applyAction' r MAttachWheels)
                 else Nothing

applyAction r map_ state MPickUpDrill = 
    let avail = boosterAvailable (robotPosition r) state Drill
    in
        if avail then Just (applyAction' r MPickUpDrill)
                 else Nothing
applyAction r map_ state MAttachDrill = 
    let avail = hasUnspentWheels r
    in
        if avail then Just (applyAction' r MAttachDrill)
                 else Nothing

applyAction r map_ state MPickUpBeacon = 
    let avail = boosterAvailable (robotPosition r) state Teleport
    in
        if avail then Just (applyAction' r MPickUpBeacon)
                 else Nothing

applyAction r map_ state MPlaceBeacon = 
    let bpos = robotPosition r
        avail = all id [
                hasUnspentBeacons r,
                not $ S.member bpos $ robotBeacons r
            ]
    in
        if avail then Just (applyAction' r MPlaceBeacon)
                 else Nothing   

applyAction r map_ state (MTeleport b) = 
    if (S.member b $ robotBeacons r)
        then Just (applyAction' r (MTeleport b))
        else Nothing

-- Expects point to be turned north initially.
applyOrientation :: Orientation -> I2 -> I2
applyOrientation N p = p
applyOrientation W (V2 x y) = V2 (-y) x
applyOrientation S (V2 x y) = V2 (-x) (-y)
applyOrientation E (V2 x y) = V2 y    (-x)

rotateLeft :: Orientation -> Orientation
rotateLeft N = W
rotateLeft W = S
rotateLeft S = E
rotateLeft E = N

rotateRight :: Orientation -> Orientation
rotateRight N = E
rotateRight W = N
rotateRight S = W
rotateRight E = S
