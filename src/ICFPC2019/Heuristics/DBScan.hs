module ICFPC2019.Heuristics.DBScan where

import Data.List.Split (splitOn)
import Data.List (union, foldl')
import Data.Bool (bool)

import Linear.V2
import ICFPC2019.Utils

type Vector a = [a]

fromI2 :: I2 -> Vector Int
fromI2 (V2 x y) = vector2 x y

toI2 :: Point Int -> I2
toI2 p = V2 x y
  where
    x:y:oth = vector p

vector2 :: Num a => a -> a -> Vector a
vector2 x y = [x, y]

vector3 :: Num a => a -> a -> a -> Vector a
vector3 x y z = [x, y, z]

vectorN :: Num a => [a] -> Vector a
vectorN = id

sumVector :: Num a => Vector a -> Vector a -> Vector a
sumVector = zipWith (+)

difference :: Num a => Vector a -> Vector a -> Vector a
difference = zipWith (-)

lengthVector :: (Num a, Enum a) => Vector a -> a
lengthVector = sum . fmap f . zip [1..]
  where f (i, x) = x ^ i

distance :: Num a => Vector a -> Vector a -> a
distance v = sum . fmap f . zip v
  where f (x1, x2) = (x1 - x2) ^ 2

data Point a = Visited    (Vector a)
               | NotVisited (Vector a)
               | Noise      (Vector a)
               deriving (Show)

instance Eq a => Eq (Point a) where
  p1 == p2 = vector p1 == vector p2

type Cluster a = (Int, [Point a])

vector :: Point a -> Vector a
vector (Visited va)    = va
vector (NotVisited va) = va
vector (Noise va)      = va

visited :: Point a -> Bool
visited (NotVisited _) = False
visited (Noise _)      = False
visited _              = True

visit :: Point a -> Point a
visit = Visited . vector

noise :: Point a -> Point a
noise = Noise . vector

dbscan :: (Ord a, Num a) => a -> Int -> [Point a] -> ([Cluster a], [Point a])
dbscan eps minPts points = let (cl, ns) = foldl constructClusers ([], []) points
                           in (zip [1..] cl, ns)
  where constructClusers (clusters, ns') c
          | or (fmap (c `elem`) clusters) = (clusters, ns')
          | otherwise  = case construction eps minPts points c of
            (Left n)           -> (clusters, n:ns')
            (Right newCluster) -> (clusters ++ [newCluster], ns')

construction :: (Ord a, Num a) => a -> Int -> [Point a] -> Point a -> Either (Point a) [Point a]
construction eps minPts points p =
  let pVisited   = visit p
      neighbours = epsNeighbourhood eps pVisited points
  in if length neighbours < minPts
   then Left (noise pVisited)
   else Right (expansion [pVisited] [] eps minPts points)

expansion :: (Ord a, Num a) => [Point a] -> [Point a] -> a -> Int -> [Point a] -> [Point a]
expansion [] pss _ _ _ = pss
expansion (p:ps) pps eps minPts points =
     let neighbours = epsNeighbourhood eps p points
         ps'        = (ps `union` filter (`notElem` pps) neighbours)
         pps'       = bool pps (p:pps) (p `notElem` pps)
     in bool [] (expansion ps' pps' eps minPts points) (length neighbours >= minPts)

epsNeighbourhood :: (Num a, Ord a) => a -> Point a -> [Point a] -> [Point a]
epsNeighbourhood eps point = filter ((>=) eps . distance (vector point) . vector)
