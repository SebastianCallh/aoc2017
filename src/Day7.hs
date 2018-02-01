{-# LANGUAGE RecordWildCards #-}

module Day7 where

import           Control.Monad        (join)
import           Data.Either          (rights)
import           Data.List            (find, partition, sortOn)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust, fromMaybe, isJust)
import           Data.Monoid          (Sum (..))
import           Data.Void
import           Debug.Trace
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

data Program = Program
  { name     :: String
  , weight   :: Int
  , supports :: [String]
  } deriving (Show)

data Tree = Node
  { label    :: String
  , value    :: Int
  , children :: [Tree]
  } deriving (Show)

instance Eq Tree where
  (==) a b = value a == value b

instance Ord Tree where
  (<=) a b = value a <= value b

parens = between (char '(') (char ')')

program :: Parser Program
program = do
  name <- many alphaNumChar
  spaceChar
  weights <- read <$> parens (many digitChar)
  supports <- option [] $ do
    spaceChar
    string "->"
    spaceChar
    many alphaNumChar `sepBy` (string ", ")
  return $ Program name weights supports

input :: IO [String]
input = lines <$> readFile "input/day7-input.txt"

mkTree :: M.Map String Program -> Program -> Tree
mkTree programs Program{..} =
  Node name weight $ mkTree programs <$> children
  where
    children = fromJust . (flip M.lookup programs) <$> supports

answer1 :: IO Program
answer1 = do
  lines <- input
  let programs = rights $ parse program "" <$> lines
  let ins  = concatMap supports programs
  let mroot = find (\(Program n _ _)-> not $ n `elem` ins) programs
  return $ fromJust mroot

answer2 :: IO Int
answer2 = do
  lines <- input
  root  <- answer1
  let progs = rights $ parse program "" <$> lines
  let programs = M.fromList [(name p, p) | p <- progs]
  let tree = mkTree programs root
  return $ unbalance tree 0

unbalance :: Tree -> Int -> Int
unbalance node val =
  case unbalancedChild node of
    Just (val', child) ->
      trace (mconcat ["Calling unbalance with ", show $ nodeSum <$> children node]) $ unbalance child val'
    Nothing    ->
      trace (mconcat ["i think that this is the value: for ", show $ val, " " , show $ nodeSum node]) $
      value node - (nodeSum node - val)

unbalancedChild :: Tree -> Maybe (Int, Tree)
unbalancedChild node =
  unbalanced $ partition (\x -> nodeSum x == (nodeSum $ head cs)) cs
  where
    cs = children node
    unbalanced (a, b) =
      if length a == length b ||
         length a == 0 ||
         length b == 0
      then Nothing
      else Just $ (val, child)
      where val = nodeSum $ tmp !! 1
            child = head tmp
            tmp = head <$> sortOn length [a, b]

isBalanced :: Tree -> Bool
isBalanced n = and $ zipWith (==) cs (tail cs)
  where cs = children n

nodeSum :: Tree -> Int
nodeSum n = value n + (getSum $ foldMap (Sum . nodeSum) $ children n)

testTree = Node "a" 5
  [ Node "b" 1
    [ Node "b1" 1 []
    , Node "b2" 1 []
    , Node "b3" 1 []
    ],
    Node "c" 1
    [ Node "b1" 2 []
    , Node "b2" 2 []
    , Node "b3" 2 []
    ],
    Node "d" 2
    [ Node "d1" 2 []
    , Node "d2" 2 []
    , Node "d3" 2 []
    ]
  ]
