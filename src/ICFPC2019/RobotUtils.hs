{-
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
-}
module ICFPC2019.RobotUtils where

import Control.Arrow
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
mapEdgeOrWall gameMap drilledCells pos = not $ checkBoundaries gameMap pos && checkObstacles gameMap drilledCells pos

numWalls :: MapArray -> Set I2 -> I2 -> Int
numWalls gameMap drilledCells pos = sum $ map fromEnum [mapEdgeOrWall gameMap drilledCells (move pos orientation)
                                                | orientation <- [E, N, W, S]]

getMapNeighbours :: MapArray -> Set I2 -> I2 -> [I2]
getMapNeighbours gameMap drilledCells pos = filter (not . mapEdgeOrWall gameMap drilledCells) allNeighbours
  where
    allNeighbours = [move pos orient | orient <- [N, E, S, W]]

sign :: Float -> Int
sign v
    | abs v < 1e-6 = 0
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

-- pointOnLine :: line pt 0 -> line pt 1 -> point
pointOnLine :: V2 Float -> V2 Float -> I2 -> Bool
pointOnLine (V2 x0 y0) (V2 x1 y1) (V2 _x _y) = 
    let x = fromIntegral _x
        y = fromIntegral _y
        a = y0 - y1
        b = x1 - x0
        c = x0 * y1 - x1 * y0
        res = a * x + b * y + c
    in abs res < 1e-6

-- +-------+-------+-------+
-- |       |  B    |       |
-- |       |   x   |       |
-- |      C|       |C'     |
-- +-------****>****-------+
-- |       *       *       | 
-- |       ^       v   x   | 
-- |       *       *    A  | 
-- +-------****<****-------+
-- |       |       |D      | 
-- |       |       |       | 
-- |       |       |       | 
-- +-------+-------+-------+
--
-- Steps:
--   1. Find C' that lies on the line
--   2. Find the triangle, in which C' is the end of one side and the start of another (e.g. CC'D)
--   3. Find out if CD intersects AB. If it does not, the line touches the corner of the cell, if it does, the line intersects with the cell
lineTouchesCellCorner :: V2 Float -> V2 Float -> [(I2, I2)] -> Bool
lineTouchesCellCorner a b sides =
    let points = fst <$> sides
        chkPointsOnAB = (pointOnLine a b) <$> points
        pointsOnAB = S.fromList $ (fst) <$> (filter (snd) $ zip points chkPointsOnAB)
    in if length pointsOnAB /= 1
        then False
        else 
            let corners = zipWith (\a b -> (a, b)) sides $ drop 1 sides ++ [head sides]
                corner = head $ filter (\((c, c'), (_, d)) -> S.member c' pointsOnAB) corners
                ((c, _), (_, d)) = corner
            in not $ segmentIntersectsLine a b c d


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

boostersInBoundingBox :: MapArray -> ProblemState -> I2 -> I2 -> Set I2
boostersInBoundingBox map_ st p1 p2 = 
    let allCells = cellsInBB p1 p2
        validCells = filter (checkMapObstacle map_) allCells
        boosters = filter (\pos -> M.member pos $ problemBoosters st) validCells
    in S.fromList boosters

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
        checkIntersections = not $ any (uncurry $ segmentIntersectsLine a b) sides
        checkCorners = lineTouchesCellCorner a b sides
    in checkCorners || checkIntersections

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
    , robotOrientation = rotateOrientation action $ robotOrientation r
    }

validRobot :: MapArray -> ProblemState -> Robot -> Bool
validRobot map_ state r =
  let rpos = robotPosition r
      drill = drillEnabled r
  in checkBoundaries map_ rpos &&
     (drill || checkObstacles map_ (problemDrilled state) rpos)

applyMoveAction' :: MapArray -> ProblemState -> Orientation -> Int -> Robot -> ([I2], Robot)
applyMoveAction' map_ state action 0 r = ([], r)
applyMoveAction' map_ state action remainingSteps r =
    let newPos =  move (robotPosition r) action
        possibleRobot = r { robotPosition = newPos }
        (nextPath, nextRobot) = applyMoveAction' map_ state action (remainingSteps-1) possibleRobot
    in if validRobot map_ state possibleRobot
       then (newPos : nextPath, nextRobot)
       else ([], r)

applyMoveAction :: MapArray -> ProblemState -> Orientation -> Maybe ([I2], Robot)
applyMoveAction map_ state@(ProblemState {..}) action = 
    let (path, newRobot) = applyMoveAction' map_ state action (speed problemRobot) problemRobot
    in if null path
       then Nothing
       else Just (path, newRobot)

useBooster :: Booster -> Robot -> Maybe Robot
useBooster booster r =
  case M.lookup booster $ robotBoosters r of
    Just 1 -> Just r { robotBoosters = M.delete booster $ robotBoosters r }
    Just n -> Just r { robotBoosters = M.insert booster (n - 1) $ robotBoosters r }
    Nothing -> Nothing

applyAction ::  MapArray -> ProblemState -> Action -> Maybe ([I2], Robot)
applyAction map_ state@(ProblemState { problemRobot = r }) action = second decrementBoosters <$> newRobot
  where newRobot =
          case action of
            MNothing -> Just ([], r)
            MUp -> applyMoveAction map_ state N
            MDown -> applyMoveAction map_ state S
            MLeft -> applyMoveAction map_ state W
            MRight -> applyMoveAction map_ state E
            MTurnRight -> Just ([robotPosition r], applyRotAction r R)
            MTurnLeft -> Just ([robotPosition r], applyRotAction r L)
            MAttachManipulator m ->
              let applyAttach r' = ([robotPosition r], r' { robotManipulators = S.insert m $ robotManipulators r })
              in if not $ S.member m $ robotManipulators r
                 then applyAttach <$> useBooster Extension r
                 else Nothing
            MAttachWheels ->
              let applyAttach r' = ([], r' { robotWheelsLeft = 50 + max 1 (robotWheelsLeft r) })
              in applyAttach <$> useBooster FastWheels r
            MAttachDrill ->
              let applyAttach r' = ([], r' { robotDrillLeft = 30 + max 1 (robotDrillLeft r) })
              in applyAttach <$> useBooster Drill r
            MPlaceBeacon ->
              let applyAttach r' = ([], r' { robotBeacons = S.insert bpos $ robotBeacons r })
                  bpos = robotPosition r
              in if not $ S.member bpos $ robotBeacons r
                 then applyAttach <$> useBooster Teleport r
                 else Nothing
            MTeleport b ->
              if S.member b $ robotBeacons r
              then Just ([b], r { robotPosition = b })
              else Nothing


-- Expects point to be turned north initially.
applyOrientation :: Orientation -> I2 -> I2
applyOrientation E p = p
applyOrientation N (V2 x y) = V2 (-y) x
applyOrientation W (V2 x y) = V2 (-x) (-y)
applyOrientation S (V2 x y) = V2 y    (-x)

revertOrientation :: Orientation -> I2 -> I2
revertOrientation E p = p
revertOrientation N (V2 x y) = V2 y    (-x)
revertOrientation W (V2 x y) = V2 (-x) (-y)
revertOrientation S (V2 x y) = V2 (-y) x

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

rotateOrientation :: Rotation -> Orientation -> Orientation
rotateOrientation L = rotateLeft
rotateOrientation R = rotateRight
