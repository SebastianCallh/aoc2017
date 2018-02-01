module Day17 where

import qualified Data.Sequence as S

import           Data.List     (foldl', scanl')

input = 345

answer1 :: Int
answer1 = s `S.index` succ i
  where (i, s) = foldl' (spinlock input) (0, S.singleton 0) [1..2017]

--answer2 :: Int
answer2 =
  foldl' (spinlock2 input) (0, 0) [1..50000000]

spinlock :: Int -> (Int, S.Seq Int) -> Int -> (Int, S.Seq Int)
spinlock n (i, s) x = (i', S.insertAt i' x s)
  where i' = succ $ (i + n) `rem` x

spinlock2 :: Int -> (Int, Int) -> Int -> (Int, Int)
spinlock2 n (i, x) y =
  if i' == 1
  then (i', y)
  else (i', x)
  where i' = succ $ (i + n) `rem` y
