module Day10 where

import           Data.List (splitAt, unfoldr)

input :: IO String
input = readFile "input/day10-input.txt"

answer1 = do
  steps <- fmap read . words <$> input :: IO [Int]
  let elems = [0..255]
  let skips = [0,1..]
  let x = zip3 steps elems skips




--  let list = unfoldr pairs [] elems


--  let thing = foldr knot elems pairs
  return steps

thingy :: ([Int], Int) -> [Int]
thingy (es, l) = undefined
  where
    rot = reverse $ take l unrot
    (keep, unrot) = splitAt l $ cycle es

--[0, 1, 2, 3, 4, 5, 6 | 0, 1, 2, 3, 4, 5....]
-- i = 4
-- l = 3
knot :: Int -> Int -> [Int] -> [Int]
knot i l elems =
  take preL wrap
  ++ reverse rot
  ++ take sufL suf
  where
    preL =  (pred i) + l - length elems
    sufL = length elems - (pred i) - length rot
    wrap = drop (length elems - i) suf
    (rot, suf) = splitAt l unrot
    unrot  = drop (pred i) $ cycle elems

--knot :: Int -> Int -> [Int] -> Int -> [Int]
--knot k i es l = undefined
--  where
--    knot = reverse . take k $ drop k es'
--    es'  = cycle es

