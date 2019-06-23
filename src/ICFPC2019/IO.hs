module ICFPC2019.IO
  ( rawProblem
  , buildSolution
  , npArray
  , mapArray
  , buildMapArray
  ) where

import Control.Monad
import Data.Functor
import Data.List
import qualified Data.Vector.Unboxed as VU
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Linear.V2
import qualified Data.ByteString.Builder as BB
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as R

import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.Raw
  
import Debug.Trace

point :: Parser I2
point = do
  _ <- char '('
  x <- decimal
  _ <- char ','
  y <- decimal
  _ <- char ')'
  return $ V2 x y

rectilinearPoly :: Parser RectilinearPoly
rectilinearPoly = point `sepBy1'` char ','

booster :: Parser Booster
booster =
  (char 'B' $> Extension)
  <|> (char 'F' $> FastWheels)
  <|> (char 'L' $> Drill)
  <|> (char 'X' $> Mysterious)
  <|> (char 'R' $> Teleport)

boosterLocation :: Parser (Booster, I2)
boosterLocation = (,) <$> booster <*> point

boosters :: Parser [(Booster, I2)]
boosters = boosterLocation `sepBy'` char ';'

obstacles :: Parser [RectilinearPoly]
obstacles = rectilinearPoly `sepBy'` char ';'

rawProblem :: Parser RawProblem
rawProblem = do
  rawMap <- rectilinearPoly
  _ <- char '#'
  rawPosition <- point
  _ <- char '#'
  rawObstacles <- obstacles
  _ <- char '#'
  rawBoosters <- boosters
  return RawProblem {..}

mapCell :: Parser Bool
mapCell =
  (char '1' $> True)
  <|> (char '0' $> False)

mapLine :: Parser (VU.Vector Bool)
mapLine = VU.fromList <$> (mapCell `sepBy1` char ' ') <* char '\n'

mapArray :: Parser MapArray
mapArray = do
  x <- decimal
  _ <- char ' '
  y <- decimal
  _ <- char '\n'
  arr <- mconcat <$> some mapLine
  unless (VU.length arr == x * y) $ fail "mapArray: invalid size"
  return $ R.fromUnboxed (V2 x y) arr

npLine :: Parser (VU.Vector Int)
npLine = VU.fromList <$> (decimal `sepBy1` char ' ') <* char '\n'

npArray :: Parser (R.Array R.U R.DIM2 Int)
npArray = do
  y <- decimal
  _ <- char ' '
  x <- decimal
  _ <- char '\n'
  arr <- mconcat <$> some npLine
  unless (VU.length arr == x * y) $ fail "npArray: invalid size"
  return $ R.fromUnboxed (Z :. y :. x) arr


buildAction :: Action -> BB.Builder
buildAction MUp = BB.char7 'W'
buildAction MDown = BB.char7 'S'
buildAction MLeft = BB.char7 'A'
buildAction MRight = BB.char7 'D'
buildAction MNothing = BB.char7 'Z'
buildAction MTurnRight = BB.char7 'E'
buildAction MTurnLeft = BB.char7 'Q'
buildAction (MAttachManipulator (V2 dx dy)) = BB.byteString "B(" <> BB.intDec dx <> BB.char7 ',' <> BB.intDec dy <> BB.char7 ')'
buildAction MAttachWheels = BB.char7 'F'
buildAction MAttachDrill = BB.char7 'L'
buildAction MPlaceBeacon = BB.char7 'R'
buildAction (MTeleport (V2 x y)) = BB.byteString "T(" <> BB.intDec x <> BB.char7 ',' <> BB.intDec y <> BB.char7 ')'

buildSolution :: [Action] -> BB.Builder
buildSolution = mconcat . map buildAction 

buildMapArray :: MapArray -> BB.Builder
buildMapArray arr =
  BB.intDec xSize <> BB.char7 ' ' <> BB.intDec ySize <> BB.char7 '\n' <>
  mconcat (map showLine [0..ySize - 1])

  where V2 xSize ySize = R.extent arr

        showCell :: Bool -> BB.Builder
        showCell True = BB.char7 '1'
        showCell False = BB.char7 '0'

        showLine :: Int -> BB.Builder
        showLine y = mconcat (intersperse (BB.char7 ' ') $ map (\x -> showCell $ arr R.! V2 x y) [0..xSize - 1]) <> BB.char7 '\n'
