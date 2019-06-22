module ICFPC2019.RobotUtils
  ( move
  , rot
  , speed
  , checkBoundaries
  , checkObstacles
  , manipulatorExtensionLocations
  , validManipulators
  , applyAction
  , applyPick
  , applyOrientation
  , rotateLeft
  , rotateRight
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M
import qualified Data.Array.Repa as R
import Linear.V2 (V2(..), crossZ)
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

move :: I2 -> Action -> I2
move (V2 x y) MUp = V2 x (y+1)
move (V2 x y) MDown = V2 x (y-1)
move (V2 x y) MRight = V2 (x+1) y
move (V2 x y) MLeft = V2 (x-1) y
move p action = p

rot :: I2 -> Action -> I2
rot (V2 x y) MTurnLeft = V2 (-y) x
rot (V2 x y) MTurnRight = V2 y (-x)
rot p action = p

checkBoundaries :: MapArray -> I2 -> Bool
checkBoundaries gameMap = R.inShapeRange (V2 0 0) (R.extent gameMap - 1)

checkObstacles :: MapArray -> I2 -> Bool
checkObstacles gameMap pos = gameMap R.! pos

sign :: Float -> Int
sign v
    | abs v < 1e-10 = 0
    | v > 0.0 = 1
    | otherwise = -1

--             * C
--  
--  ---*-------------*
--     A             B
--
--         *D
-- sign(AC x AB) != sign(AD x AB)
segmentIntersectsLine :: V2 Float -> V2 Float -> I2 -> I2 -> Bool
segmentIntersectsLine a b (V2 xc yc) (V2 xd yd) =
    let c = V2 (fromIntegral xc) (fromIntegral yc)
        d = V2 (fromIntegral xd) (fromIntegral yd)
        ab = b - a :: V2 Float
        ad = d - a :: V2 Float
        ac = c - a :: V2 Float
        acxab = (crossZ ac ab) :: Float
        adxab = (crossZ ad ab) :: Float
    in sign acxab /= sign adxab

obstaclesInBoundingBox :: MapArray -> I2 -> I2 -> Set I2
obstaclesInBoundingBox map_ (V2 x1 y1) (V2 x2 y2) = 
    let allCells = [(V2 x y) | x <- [x1..x2],
                               y <- [y1..y2]
                   ]
        obstacles = filter (checkObstacles map_) allCells
    in S.fromList obstacles

-- cellToRect :: cell -> sides
cellToRect :: I2 -> [(I2, I2)]
cellToRect (V2 x y) =
    let p0 = V2 x y
        p1 = V2 x (y+1)
        p2 = V2 (x+1) (y+1)
        p3 = V2 (x+1) y
    in [(p0, p1), (p1, p2), (p2, p3), (p3, p1)]

--checkCellVisibility' :: src -> dst -> obstacle rect -> bool 
checkCellVisibility' :: I2 -> I2 -> [(I2, I2)] -> Bool
checkCellVisibility' (V2 xa ya) (V2 xb yb) sides = 
    let a = V2 (0.5 + fromIntegral xa) (0.5 + fromIntegral ya) :: V2 Float
        b = V2 (0.5 + fromIntegral xb) (0.5 + fromIntegral yb) :: V2 Float
    in not $ any (\(c, d) -> segmentIntersectsLine a b c d) sides

--checkCellVisibility :: map -> src -> dst -> bool
checkCellVisibility :: MapArray -> I2 -> I2 -> Bool
checkCellVisibility map_ src@(V2 x0 y0) dst@(V2 x1 y1) = 
    let obstacles = obstaclesInBoundingBox map_ src dst
        obstRects = cellToRect <$> (S.toList obstacles)
    in all id $ (checkCellVisibility' src dst) <$> obstRects

manipulatorExtensionLocations' :: I2 -> Set I2
manipulatorExtensionLocations' (V2 x y) = S.fromList [V2 (x+1) y, V2 (x-1) y, V2 x (y+1), V2 x (y-1)]

manipulatorExtensionLocations :: Set I2 -> Set I2
manipulatorExtensionLocations manips = S.difference (foldr1 (S.union) $ map manipulatorExtensionLocations' $ S.toList manips) manips

--validManipulators :: map -> pivot -> manipulators -> valid manipulators
validManipulators :: MapArray -> I2 -> Set I2 -> Set I2
validManipulators map_ pivot manips = 
    let unfolded = filter (checkBoundaries map_) $ S.toList manips
        visible = filter (checkCellVisibility map_ pivot) unfolded
    in S.fromList visible

decrementBoosters :: Robot -> Robot
decrementBoosters r =
  r { robotWheelsLeft = max 0 $ robotWheelsLeft r - 1,
      robotDrillLeft = max 0 $ robotDrillLeft r - 1
    }

applyMoveAction :: Robot -> Action -> Int -> Robot
applyMoveAction r action s =
  r { robotPosition = move (robotPosition r) action
    }

applyRotAction :: Robot -> Action -> Robot
applyRotAction r action =
  decrementBoosters $ r { robotManipulators = S.fromList $ map (\m -> rot m action) $ S.toList $ robotManipulators r
                        }

applyPick' :: Robot -> PickAction -> Robot
applyPick' r MPickUpManipulator = r {
    robotUnspentManips = 1 + robotUnspentManips r
}
applyPick' r MPickUpWheels = r {
    robotUnspentWheels = 1 + robotUnspentWheels r
}
applyPick' r MPickUpDrill = r {
    robotUnspentDrills = 1 + robotUnspentDrills r
}
applyPick' r MPickUpBeacon = r {
    robotUnspentBeacons = 1 + robotUnspentBeacons r
}

applyAction' :: Robot -> Action -> Robot
applyAction' r MNothing = decrementBoosters r
--applyAction' r MUp = applyMoveAction r MUp
--applyAction' r MDown = applyMoveAction r MDown
--applyAction' r MLeft = applyMoveAction r MLeft
--applyAction' r MRight = applyMoveAction r MRight
applyAction' r MTurnLeft = applyRotAction r MTurnLeft
applyAction' r MTurnRight = applyRotAction r MTurnRight
applyAction' r (MAttachManipulator m) = decrementBoosters $ r {
    robotManipulators = S.insert m $ robotManipulators r,
    robotUnspentManips = max 0 $ robotUnspentManips r - 1
}
applyAction' r MAttachWheels = decrementBoosters $ r {
    robotWheelsLeft = max 50 $ robotWheelsLeft r,
    robotUnspentWheels = max 0 $ robotUnspentWheels r - 1
}
applyAction' r MAttachDrill = decrementBoosters $ r {
    robotDrillLeft = max 30 $ robotDrillLeft r,
    robotUnspentDrills = max 0 $ robotUnspentDrills r - 1
}
applyAction' r MPlaceBeacon = decrementBoosters $ r {
    robotBeacons = S.insert (robotPosition r) $ robotBeacons r,
    robotUnspentBeacons = max 0 $ robotUnspentBeacons r - 1
}
applyAction' r (MTeleport b) = decrementBoosters $ r {
    robotPosition = b
}

validateRobot :: MapArray -> ProblemState -> Robot -> Maybe Robot
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

applyValidMoveAction' :: MapArray -> ProblemState -> Action -> Int -> Robot -> Robot
applyValidMoveAction' map_ state action 0 r = r
applyValidMoveAction' map_ state action remainingSteps r = 
    let possibleRobot = applyMoveAction r action 1
        validRobot = validateRobot map_ state possibleRobot
    in
        case validRobot of
            Just r' -> applyValidMoveAction' map_ state action (remainingSteps-1) r'
            Nothing -> r

applyValidMoveAction :: MapArray -> ProblemState -> Action -> Robot -> Maybe Robot
applyValidMoveAction map_ state action r = 
    let newRobot = applyValidMoveAction' map_ state action (speed r) r
    in
        if (robotPosition newRobot /= robotPosition r)
            then Just $ decrementBoosters newRobot
            else Nothing

boosterAvailable :: I2 -> ProblemState -> Booster -> Bool
boosterAvailable pos state bst =
    let boosters = problemBoosters state
    in
        if M.member pos boosters
            then S.member bst $ boosters M.! pos
            else False

applyPick :: Robot -> MapArray -> ProblemState -> PickAction -> Maybe Robot
applyPick r map_ state MPickUpManipulator =
    let avail = boosterAvailable (robotPosition r) state Extension
    in
        if avail then Just (applyPick' r MPickUpManipulator)
                 else Nothing
applyPick r map_ state MPickUpWheels =
    let avail = boosterAvailable (robotPosition r) state FastWheels
    in
        if avail then Just (applyPick' r MPickUpWheels)
                 else Nothing
applyPick r map_ state MPickUpDrill =
    let avail = boosterAvailable (robotPosition r) state Drill
    in
        if avail then Just (applyPick' r MPickUpDrill)
                 else Nothing
applyPick r map_ state MPickUpBeacon =
    let avail = boosterAvailable (robotPosition r) state Teleport
    in
        if avail then Just (applyPick' r MPickUpBeacon)
                 else Nothing

applyAction :: Robot -> MapArray -> ProblemState -> Action -> Maybe Robot
applyAction r map_ state MNothing = Just (applyAction' r MNothing)
applyAction r map_ state MUp = applyValidMoveAction map_ state MUp r
applyAction r map_ state MDown = applyValidMoveAction map_ state MDown r
applyAction r map_ state MLeft = applyValidMoveAction map_ state MLeft r
applyAction r map_ state MRight = applyValidMoveAction map_ state MRight r
applyAction r map_ state MTurnRight = Just (applyAction' r MTurnRight)
applyAction r map_ state MTurnLeft = Just (applyAction' r MTurnLeft)
applyAction r map_ state (MAttachManipulator m) = 
    let avail = hasUnspentManips r
    in
        if avail then Just (applyAction' r (MAttachManipulator m))
                 else Nothing

applyAction r map_ state MAttachWheels = 
    let avail = hasUnspentWheels r
    in
        if avail then Just (applyAction' r MAttachWheels)
                 else Nothing

applyAction r map_ state MAttachDrill = 
    let avail = hasUnspentDrills r
    in
        if avail then Just (applyAction' r MAttachDrill)
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
    if S.member b $ robotBeacons r
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
