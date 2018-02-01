module Day11 where

import           Data.List            (maximum)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

newtype Step = Step (Int, Int)
  deriving (Show)

input :: IO String
input = readFile "input/day11-input.txt"

answer1 = do
  ss <- parse steps "" <$> input
  print $ distance . foldl1 addStep <$> ss

answer2 = do
  ss <- parse steps "" <$> input
  print $  maximum . fmap distance . scanl1 addStep <$> ss

steps :: Parser [Step]
steps =  step `sepBy` char ','

step :: Parser Step
step = choice
  [ string "ne" *> pure (Step ( 1, 0))
  , string "sw" *> pure (Step (-1, 0))
  , string "se" *> pure (Step ( 1,-1))
  , string "nw" *> pure (Step (-1, 1))
  , string "n"  *> pure (Step ( 0,-1))
  , string "s"  *> pure (Step ( 0, 1))
  ]

distance :: Step -> Int
distance (Step (x, y)) =
  (abs x + abs y + abs (x + y)) `div` 2

addStep :: Step -> Step -> Step
addStep (Step (x, y)) (Step (z, w)) =
  Step (x + z, y + w)

