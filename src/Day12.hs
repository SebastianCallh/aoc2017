{-# LANGUAGE BangPatterns #-}

module Day12 where

import qualified Data.Graph           as G
import qualified Data.Map             as M

import           Data.Either          (lefts, rights)
import           Data.Maybe           (fromJust)
import           Data.Monoid          (Sum (..))
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String


number :: Parser Int
number = read <$> many digitChar

node :: Parser (Int, Int, [Int])
node = do
  n <- number
  _ <- string " <-> "
  ns <- number `sepBy` string ", "
  return $ (n, n, ns)

input :: IO String
input = readFile "input/day12-input.txt"

answer1 :: IO Int
answer1 = do
  nodes <- rights . fmap (parse node "") . lines <$> input
  let (graph, _, _) = G.graphFromEdges nodes
  return . length $ G.reachable graph 0

answer2 :: IO Int
answer2 = do
  nodes <- rights . fmap (parse node "") . lines <$> input
  let (graph, _, _) = G.graphFromEdges nodes
  return . length $ G.components graph


size :: M.Map Int [Int] -> Int -> Int
size !graph !root =
  succ . getSum $ foldMap (Sum . size graph) children
  where
    children =  M.findWithDefault [] root graph
