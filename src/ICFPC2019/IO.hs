module ICFPC2019.IO
  ( problem
  ) where

import Data.Functor
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Linear.V2

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

problem :: Parser RawProblem
problem = do
  rawMap <- rectilinearPoly
  _ <- char '#'
  rawPosition <- point
  _ <- char '#'
  rawObstacles <- obstacles
  _ <- char '#'
  rawBoosters <- boosters
  return RawProblem {..}
