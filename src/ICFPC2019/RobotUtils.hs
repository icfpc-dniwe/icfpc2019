module ICFPC2019.RobotUtils
  ( manipulatorExtensionLocations
  , validManipulators
  , applyAction
  , applyOrientation
  , rotateLeft
  , rotateRight
  , numWalls
  , drillEnabled
  , checkMapObstacle
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M
import qualified Data.Array.Repa as R
import Linear.V2 (V2(..), crossZ)
import Data.Maybe
import Debug.Trace

import ICFPC2019.Types
import ICFPC2019.Utils

speed :: Robot -> Int
speed Robot {..}
  | robotWheelsLeft > 0 = 2
  | otherwise = 1

drillEnabled :: Robot -> Bool
drillEnabled Robot {..} = robotDrillLeft > 0

move :: I2 -> Orientation -> I2
move (V2 x y) N = V2 x (y+1)
move (V2 x y) S = V2 x (y-1)
move (V2 x y) E = V2 (x+1) y
move (V2 x y) W = V2 (x-1) y

rot :: I2 -> Rotation -> I2
rot (V2 x y) L = V2 (-y) x
rot (V2 x y) R = V2 y (-x)

checkBoundaries :: MapArray -> I2 -> Bool
checkBoundaries gameMap = R.inShapeRange (V2 0 0) (R.extent gameMap - 1)

checkMapObstacle :: MapArray -> I2 -> Bool
checkMapObstacle gameMap pos
    | checkBoundaries gameMap pos = gameMap R.! pos
    | otherwise = False

checkObstacles :: MapArray -> Set I2 -> I2 -> Bool
checkObstacles gameMap drilledCells pos =
    let nonObstacle = checkMapObstacle gameMap pos
        drilled = S.member pos drilledCells
    in or [nonObstacle, drilled]

mapEdgeOrWall :: MapArray -> Set I2 -> I2 -> Bool
mapEdgeOrWall gameMap drilledCells pos = not $ checkBoundaries gameMap pos || checkObstacles gameMap drilledCells pos

numWalls :: MapArray -> Set I2 -> I2 -> Int
numWalls gameMap drilledCells pos = sum $ map fromEnum [mapEdgeOrWall gameMap drilledCells (move pos orientation)
                                                | orientation <- [E, N, W, S]]

sign :: Float -> Int
sign v
    | abs v < 1e-7 = 0
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
        acxab = crossZ ac ab :: Float
        adxab = crossZ ad ab :: Float
    in sign acxab /= sign adxab

cellsInBB :: I2 -> I2 -> [I2]
cellsInBB (V2 x1 y1) (V2 x2 y2)
    | x1 < x2 && y1 < y2 = [V2 x y | x <- [x1..x2], y <- [y1..y2]]
    | x1 < x2 && y1 >= y2 = [V2 x y | x <- [x1..x2], y <- [y2..y1]]
    | x1 >= x2 && y1 < y2 = [V2 x y | x <- [x2..x1], y <- [y1..y2]]
    | otherwise           = [V2 x y | x <- [x2..x1], y <- [y2..y1]]

obstaclesInBoundingBox :: MapArray -> Set I2 -> I2 -> I2 -> Set I2
obstaclesInBoundingBox map_ drilledCells p1 p2 = 
    let allCells = cellsInBB p1 p2
        obstacles = filter (not . checkObstacles map_ drilledCells) allCells
    in S.fromList obstacles

-- cellToRect :: cell -> sides
cellToRect :: I2 -> [(I2, I2)]
cellToRect (V2 x y) =
    let p0 = V2 x y
        p1 = V2 x (y+1)
        p2 = V2 (x+1) (y+1)
        p3 = V2 (x+1) y
    in [(p0, p1), (p1, p2), (p2, p3), (p3, p0)]

--checkCellVisibility' :: src -> dst -> obstacle rect -> bool 
checkCellVisibility' :: I2 -> I2 -> [(I2, I2)] -> Bool
checkCellVisibility' (V2 xa ya) (V2 xb yb) sides = 
    let a = V2 (0.5 + fromIntegral xa) (0.5 + fromIntegral ya) :: V2 Float
        b = V2 (0.5 + fromIntegral xb) (0.5 + fromIntegral yb) :: V2 Float
    in not $ any (uncurry $ segmentIntersectsLine a b) sides

--checkCellVisibility :: map -> drilled cells -> src -> dst -> bool
checkCellVisibility :: MapArray -> Set I2 -> I2 -> I2 -> Bool
checkCellVisibility map_ drilledCells src@(V2 x0 y0) dst@(V2 x1 y1) = 
    let obstacles = obstaclesInBoundingBox map_ drilledCells src dst
        obstRects = cellToRect <$> S.toList obstacles
    in and $ checkCellVisibility' src dst <$> obstRects

manipulatorExtensionLocations' :: I2 -> Set I2
manipulatorExtensionLocations' (V2 x y) = S.fromList [V2 (x+1) y, V2 (x-1) y, V2 x (y+1), V2 x (y-1)]

manipulatorExtensionLocations :: Set I2 -> Set I2
manipulatorExtensionLocations manips = S.difference (foldr1 S.union $ map manipulatorExtensionLocations' $ S.toList manips) manips

--validManipulators :: map -> drilled cells -> pivot -> manipulators -> valid manipulators
validManipulators :: MapArray -> Set I2 -> I2 -> Set I2 -> [I2]
validManipulators map_ drilledCells pivot manips = 
    let manips' = map (+ pivot) $ S.toList manips
        unfolded = filter (checkBoundaries map_) manips'
        visible = filter (checkCellVisibility map_ drilledCells pivot) unfolded
    in visible

decrementBoosters :: Robot -> Robot
decrementBoosters r =
  r { robotWheelsLeft = max 0 $ robotWheelsLeft r - 1,
      robotDrillLeft = max 0 $ robotDrillLeft r - 1
    }

applyRotAction :: Robot -> Rotation -> Robot
applyRotAction r action =
  r { robotManipulators = S.fromList $ map (`rot` action) $ S.toList $ robotManipulators r
    }

validRobot :: MapArray -> Robot -> Bool
validRobot map_ r = 
  let rpos = robotPosition r
      drill = drillEnabled r
  in checkBoundaries map_ rpos &&
     (drill || checkObstacles map_ (robotDrilled r) rpos)

applyMoveAction' :: MapArray -> ProblemState -> Orientation -> Int -> Robot -> (Bool, Robot)
applyMoveAction' map_ state action 0 r = (False, r)
applyMoveAction' map_ state action remainingSteps r =
    let newPos =  move (robotPosition r) action
        newDrilled = S.union (robotDrilled r) $
            if drillEnabled r
                then S.fromList $ filter (\p -> not $ checkMapObstacle map_ p) [newPos]
                else S.empty
        possibleRobot = r { robotPosition = newPos, robotDrilled = newDrilled }
        (_, nextRobot) = applyMoveAction' map_ state action (remainingSteps-1) possibleRobot
    in if validRobot map_ possibleRobot
       then (True, nextRobot)
       else (False, r)

applyMoveAction :: MapArray -> ProblemState -> Orientation -> Maybe Robot
applyMoveAction map_ state@(ProblemState {..}) action = 
    let (moved, newRobot) = applyMoveAction' map_ state action (speed problemRobot) problemRobot
    in
      if moved
      then Just newRobot
      else Nothing

useBooster :: Booster -> Robot -> Maybe Robot
useBooster booster r =
  case M.lookup booster $ robotBoosters r of
    Just 1 -> Just r { robotBoosters = M.delete booster $ robotBoosters r }
    Just n -> Just r { robotBoosters = M.insert booster (n - 1) $ robotBoosters r }
    Nothing -> Nothing

applyAction ::  MapArray -> ProblemState -> Action -> Maybe Robot
applyAction map_ state@(ProblemState { problemRobot = r }) action = decrementBoosters <$> newRobot
  where newRobot =
          case action of
            MNothing -> Just r
            MUp -> applyMoveAction map_ state N
            MDown -> applyMoveAction map_ state S
            MLeft -> applyMoveAction map_ state W
            MRight -> applyMoveAction map_ state E
            MTurnRight -> Just $ applyRotAction (problemRobot state) R
            MTurnLeft -> Just $ applyRotAction (problemRobot state) L
            MAttachManipulator m ->
              let applyAttach r' = r' { robotManipulators = S.insert m $ robotManipulators r' }
              in if not $ S.member m $ robotManipulators r
                 then applyAttach <$> useBooster Extension r
                 else Nothing
            MAttachWheels ->
              let applyAttach r' = r' { robotWheelsLeft = 50 + max 1 (robotWheelsLeft r) }
              in applyAttach <$> useBooster FastWheels r
            MAttachDrill ->
              let applyAttach r' = r' { robotDrillLeft = 30 + max 1 (robotDrillLeft r) }
              in applyAttach <$> useBooster Drill r
            MPlaceBeacon ->
              let applyAttach r' = r' { robotBeacons = S.insert bpos $ robotBeacons r' }
                  bpos = robotPosition r
              in if not $ S.member bpos $ robotBeacons r
                 then applyAttach <$> useBooster Teleport r
                 else Nothing
            MTeleport b ->
              if S.member b $ robotBeacons r
              then Just r { robotPosition = b }
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
