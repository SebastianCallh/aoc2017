module Day13 where

import           Data.Either          (rights)
import           Data.List            (findIndex)
import           Data.Maybe           (fromJust)
import           Data.Monoid          (Sum (..))
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

data Layer
  = Layer Int Int
  | Empty Int
  deriving (Show)

input :: IO String
input = readFile "input/day13-input.txt"

layer :: Parser Layer
layer = do
  i <- read <$> many digitChar
  _ <- string ": "
  d <- read <$> many digitChar
  return $ Layer i d

answer1 :: IO Int
answer1 = do
  ls <- rights <$> fmap (parse layer "") . lines <$> input
  return $ severities ls

answer2 :: IO Int
answer2 = do
  ls  <- rights <$> fmap (parse layer "") . lines <$> input
  let dls = iterate (fmap delay) ls
  return . fromJust $ findIndex ((==) 0 . severities) dls

delay :: Layer -> Layer
delay (Layer i d)  =
  Layer (succ i) d

severities :: [Layer] -> Int
severities =
  getSum . foldMap (Sum . severity)

severity :: Layer -> Int
severity (Layer i d) =
  if   i `rem` k == 0
  then i * d
  else 0
  where k = 2*(d - 1)
