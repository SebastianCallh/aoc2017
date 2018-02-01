module Day4 where

import           Data.List       (sort)
import           Data.List.Split (splitOn)
import qualified Data.Set        as Set

input :: IO [String]
input = lines <$> readFile "day4-input.txt"

answer1 :: IO Int
answer1 = length <$> (filter isValid <$> input)

answer2 :: IO Int
answer2 = length <$> (filter isValid2 <$> input)

isValid :: String -> Bool
isValid line =
  length words == Set.size (Set.fromList words)
  where words = splitOn " " line

isValid2 :: String -> Bool
isValid2 line =
  length words == Set.size (Set.fromList words)
  where words = sort <$> splitOn " " line

