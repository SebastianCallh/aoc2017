{-# LANGUAGE BangPatterns #-}

module Day5 where

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

input :: IO [String]
input = lines <$> readFile "input/day5-input.txt"

answer1 :: IO Int
answer1 = do
  lines  <- V.fromList . fmap read <$> input :: IO (V.Vector Int)
  v      <- V.thaw lines
  jumps v 0 0

answer2 :: IO Int
answer2 = do
  lines  <- V.fromList . fmap read <$> input :: IO (V.Vector Int)
  v      <- V.thaw lines
  strangeJumps v 0 0

jumps :: M.IOVector Int -> Int -> Int -> IO Int
jumps  !instrs !n !k
  | M.length instrs <= n = pure k
  | 0               >  n = pure k
  | otherwise = do
      i <- M.read instrs n
      M.write instrs n (succ i)
      jumps instrs (n + i) (succ k)

strangeJumps :: M.IOVector Int -> Int -> Int -> IO Int
strangeJumps !instrs !n !k
  | M.length instrs <= n = pure k
  | 0               >  n = pure k
  | otherwise = do
      i <- M.read instrs n
      M.unsafeWrite instrs n $ if i >= 3
        then pred i
        else succ i
      strangeJumps instrs (n + i) (succ k)
