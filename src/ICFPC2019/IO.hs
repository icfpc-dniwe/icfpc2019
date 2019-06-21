module ICFPC2019.IO
  ( rawProblem
  , buildSolution
  ) where

import Data.Functor
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Linear.V2
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

buildSolution :: [Action] -> BB.Builder
buildSolution = mconcat . map buildAction 
