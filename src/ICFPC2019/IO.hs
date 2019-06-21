module ICFPC2019.IO
  ( rawProblem
  , buildSolution
  ) where

import Data.Functor
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Linear.V2
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB

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
rectilinearPoly = point `sepBy'` char ','

booster :: Parser Booster
booster =
  (char 'B' $> Extension)
  <|> (char 'F' $> FastWheels)
  <|> (char 'L' $> Drill)
  <|> (char 'X' $> Mysterious)

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

buildAction :: Action -> BB.Builder
buildAction MUp = char7 'W'
buildAction MDown = char7 'S'
buildAction MLeft = char7 'A'
buildAction MRight = char7 'D'
buildAction MNothing = char7 'Z'
buildAction MTurnRight = char7 'E'
buildAction MTurnLeft = char7 'Q'
buildAction MAttachManipulator (V2 dx dy) = byteString "B(" <> intDec dx <> char7 ',' <> intDec dy <> char7 ')'
buildAction MAttachWheels = char7 'F'

buildSolution :: [Action] -> BL.ByteString
buildSolution = 
