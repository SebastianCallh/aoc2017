{-# LANGUAGE BangPatterns #-}

module Day6 where

import qualified Data.Set                    as S
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

import           Data.List                   (elemIndex)
import           Data.Maybe                  (fromJust)

input :: IO String
input = readFile "input/day6-input.txt"

answer1 :: IO Int
answer1 = do
  ns <- fmap read . lines <$> input
  let v = V.fromList ns
  let s = S.empty
  m  <- V.thaw v
  fst <$> distribute m s [] 0 0 0

answer2 :: IO Int
answer2 = do
  ns <- fmap read . lines <$> input
  let v = V.fromList ns
  let s = S.empty
  m  <- V.thaw v
  snd <$> distribute m s [] 0 0 0

distribute
  :: M.IOVector Int
  -> S.Set (V.Vector Int)
  -> [V.Vector Int]
  -> Int
  -> Int
  -> Int
  -> IO (Int, Int)
distribute !ns !s !is !k !i !n = do
  v <- V.freeze ns
  if n == 0
  then
    if S.member v s
    then return (k, l v)
    else do
      (i', n') <- largest ns
      M.write ns i' 0
      distribute ns (S.insert v s) (v:is) (succ k) (inc i') n'
  else do
    M.modify ns succ i
    distribute ns s is k (inc i) (pred n)
  where
    inc x = succ x `rem` M.length ns
    l v = fromJust $ (-) (length is) <$> elemIndex v (reverse is)

largest :: M.IOVector Int -> IO (Int, Int)
largest !m = do
  v <- V.freeze m
  let x = V.maximum v
  let i = fromJust $ V.elemIndex x v
  return (i, x)
