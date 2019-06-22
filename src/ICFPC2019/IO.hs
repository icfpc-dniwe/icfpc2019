module ICFPC2019.IO
  ( rawProblem
  , buildSolution
  , mapArray
  , buildMapArray
  ) where

import Data.Functor
import Data.Vector.Unboxed as U
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Linear.V2
import qualified Data.ByteString.Builder as BB
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as R

import ICFPC2019.Types
import ICFPC2019.Utils
import ICFPC2019.Raw
  
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
mapLine = do
  VU.fromList <$> (mapCell `sepBy1` char ' ')

mapArray :: Parser MapArray
mapArray = do
  x <- decimal
  _ <- char ' '
  y <- decimal
  _ <- char '\n'
  arr <- (mconcat . reverse) <$> (mapArray `sepBy1` char '\n')
  unless (VU.length arr == x * y) $ fail "mapArray: invalid size"
  return $ R.fromUnboxed (V2 x y) arr


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
  BB.intDec xSyze <> BB.char7 ' ' <> BB.intDec ySize <> BB.char7 '\n' <>
  mconcat (map showLine [ySize - 1,ySize - 2..0])

  where V2 xSize ySize = R.extent arr

        showLine y = mconcat (intersperse ' ' $ map (\x -> BB.char7 $ if arr R.! V2 x y then '1' else '0') [0..xSize - 1]) <> BB.char7 '\n'
