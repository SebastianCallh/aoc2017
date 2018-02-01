{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}

module Day18 where

import qualified Data.Map             as M
import qualified Data.Sequence        as S

import           Data.Either          (lefts, rights)
import           Data.List            (find, iterate, scanl')
import           Data.Maybe           (isJust)
import           Data.Void            (Void)
import           Prelude              hiding (mod, snd)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

type Vars = M.Map Char Int

newtype Expr = Expr { execute :: (Vars, Int) -> (Vars, Int) }

type Arg = (Vars -> Int)

type Sign = (Int -> Int)

input :: IO String
input = readFile "input/day18-input.txt"

expr :: Parser Expr
expr = choice
  [ set
  , mul
  , add
  , mod
  , rcv
  , snd
  , jgz
  ]

set :: Parser Expr
set = mkExpr "set" $ \reg arg (vars, i) ->
  (M.insert reg arg vars, succ i)

mul :: Parser Expr
mul = mkExpr "mul" $ \reg arg (vars, i) ->
  (M.adjust (*arg) reg vars, succ i)

add :: Parser Expr
add = mkExpr "add" $ \reg arg (vars, i) ->
  (M.adjust (+arg) reg vars, succ i)

mod :: Parser Expr
mod = mkExpr "mod" $ \reg arg (vars, i) ->
  (M.adjust (`rem`arg) reg vars, succ i)

-- x reg is the last recovered sound
rcv :: Parser Expr
rcv = do
  _   <- string "rcv"
  space
  reg <- letterChar
  return $ Expr $ \(vars, i) ->
    let val = M.findWithDefault 0 reg vars
    in if val /= 0
    then (M.insert 'x' val vars, succ i)
    else (vars, succ i)

-- z reg is the last played sound
snd :: Parser Expr
snd = do
  _   <- string "snd"
  space
  reg <- letterChar
  return $ Expr $ \(vars, i) ->
    (M.insert 'z' (M.findWithDefault 0 reg vars) vars, succ i)

jgz :: Parser Expr
jgz = do
  _   <- string "jgz"
  space
  a <- try val <|> arg
  space
  b <- try val <|> arg
  return $ Expr $ \(vars, i) ->
    let x = a vars
        y = b vars
    in if x /= 0
    then (vars, i + y)
    else (vars, succ i)

sign :: Parser Sign
sign = choice
  [ string "-" *> pure negate
  , string ""  *> pure id
  ]

val :: Parser Arg
val = do
  sig <- sign
  reg <- letterChar
  return $ sig . M.findWithDefault 0 reg

arg :: Parser Arg
arg = do
  sig  <- sign
  digs <- many digitChar
  return $ sig . const (read $ digs)

mkExpr :: String -> (Char -> Int -> (Vars, Int) -> (Vars, Int)) -> Parser Expr
mkExpr name f = do
  _   <- string name
  space
  reg <- letterChar
  space
  x <- try val <|> arg
  return $ Expr $ \env@(vars, _) ->
      let v = x vars
      in f reg v env

--answer1 :: IO Int
answer1 = do
  ls    <- lines <$> input
  let exprs = S.fromList . rights $ parse expr "" <$> ls
  let res = iterate (run exprs) (M.empty, 0)
  let x = find (isJust . M.lookup 'x' . fst) res
  return $ M.lookup 'x' . fst <$> x

run :: S.Seq Expr -> (Vars, Int) -> (Vars, Int)
run !exps !env@(_, i) = run exps (execute (exps `S.index` i) env)
