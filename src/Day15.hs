module Day15 where

import           Data.Function (on)
import           Data.Int      (Int16, Int64)

input :: (Int64, Int64)
input = (679, 771)

answer1 :: Int
answer1 = do
  let (seedA, seedB) = input
  let streamA = iterate genA seedA
  let streamB = iterate genB seedB
  let seq = zipWith ((,) `on` int16) streamA streamB
  length $ filter (uncurry (==)) $ take 40000000 $ seq

answer2 :: Int
answer2 = do
  let (seedA, seedB) = input
  let streamA = filter ((==0) . (`rem` 4)) $ iterate genA seedA
  let streamB = filter ((==0) . (`rem` 8)) $ iterate genB seedB
  let seq = zipWith ((,) `on` int16) streamA streamB
  length $ filter (uncurry (==)) $ take 5000000 $ seq

d :: Int64
d = 2147483647

genA :: Int64 -> Int64
genA x = x*16807 `rem` d

genB :: Int64 -> Int64
genB x = x*48271 `rem` d

int16 :: Int64 -> Int16
int16 = fromIntegral

