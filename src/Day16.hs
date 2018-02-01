{-# LANGUAGE BangPatterns #-}

module Day16 where

import qualified Data.Sequence        as S

import           Data.Either          (either, isRight, rights)
import           Data.Function        (on)
import           Data.List            (elemIndex, foldl', repeat, scanl',
                                       splitAt)
import           Data.Maybe           (fromJust)
import           Data.Sequence        ((><))
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

type Order = S.Seq Char

newtype Move = Move { perform :: (Order -> Order) }

moves :: Parser [Move]
moves = choice [spin, exchange, partner] `sepBy` char ','

spin :: Parser Move
spin = do
  char 's'
  a <- read <$> many digitChar
  return . Move $ \o ->
    let (pre, suf) = S.splitAt (S.length o - a) o
    in suf >< pre

exchange :: Parser Move
exchange = do
  char 'x'
  i <- read <$> many digitChar
  char '/'
  j <- read <$> many digitChar
  return $ Move (swap i j)

partner :: Parser Move
partner = do
  char 'p'
  a <- letterChar
  char '/'
  b <- letterChar
  return . Move $ \o ->
    let i = fromJust $ S.elemIndexL a o
        j = fromJust $ S.elemIndexL b o
    in swap i j o

swap :: Int -> Int -> Order -> Order
swap i j o =
  S.update i y . S.update j x $ o
  where
    (x, y) = (o `S.index` i, o `S.index` j)

input :: IO String
input = readFile "input/day16-input.txt"

answer1 = do
  ms <- parse moves "" <$> input
  let ms' = either (const []) id ms
  let ps = S.fromList ['a'..'p']
  let ps' = dance ps ms'
  return ps'

answer1johan = do
  ms <- parse moves "" <$> readFile "input/day16-input-johan.txt"
  let ms' = either (const []) id ms
  let ps = S.fromList ['a'..'p']
  let ps' = dance ps ms'
  return ps'

answer2johan = n --ps !! i
  where
    i  = 1000000000 `rem` succ n
    n  = fromJust . elemIndex p $ tail ps
    ps = iterate dancejohan' p
    p  = ['a'..'p']

dancejohan' [!a,!b,!c,!d,!e,!f,!g,!h,!i,!j,!k,!l,!m,!n,!o,!p]
  = [b,k,g,c,d,e,f,i,h,o,l,n,p,m,j,a]

answer2 = do
  ms <- parse moves "" <$> input
  let ms' = either (const []) id ms
  let ps = S.fromList ['a'..'p']
  let ps' = scanl' dance ps $ repeat ms'
  let p  = S.fromList ['a'..'p']
  let n  = fromJust . elemIndex p $ tail ps'
  let i  = 1000000000 `rem` succ n
  return $ ps' !! i


dance' [!a,!b,!c,!d,!e,!f,!g,!h,!i,!j,!k,!l,!m,!n,!o,!p]
     = [ n, a, m, d, g, k, b, h, i, f, p, c, e, l, o, j]





-- klcdopahibefmngj

dance :: S.Seq Char -> [Move] -> S.Seq Char
dance = foldl' (\p m -> perform m $ p)
