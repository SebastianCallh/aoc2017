{-# LANGUAGE LambdaCase #-}

module Day8 where

import           Prelude              hiding (EQ, GT, LT)

import qualified Data.Map             as M

import           Data.Char            (digitToInt)
import           Data.Either          (lefts, rights)
import           Data.List            (scanl', sort, sortOn)
import           Data.Maybe           (fromJust)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

type Reg = String
type Env = M.Map Reg Int

newtype Cond = Cond { check :: (Env -> Bool) }

newtype Op = Op { run :: (Env -> Env) }

newtype Instr = Instr { exec :: (Env -> Env) }

input :: IO String
input = readFile "input/day8-input.txt"


number :: Parser Int
number = do
  sign   <- option "" $ string "-"
  digits <- many digitChar
  return . read $ mconcat [sign, digits]

reg :: Parser Reg
reg = many alphaNumChar

cond :: Parser Cond
cond = do
  r <- reg
  space
  c <- infxCond
  space
  n <- number
  return $ Cond (\m -> M.findWithDefault 0 r m `c` n)

infxCond :: Parser (Int -> Int -> Bool)
infxCond = (string ">=" *> pure (>=))
       <|> (string "<=" *> pure (<=))
       <|> (string "==" *> pure (==))
       <|> (string "!=" *> pure (/=))
       <|> (string ">"  *> pure (>))
       <|> (string "<"  *> pure (<))

op :: Parser Op
op = do
  r <- reg
  space
  o <- infxOp
  space
  n <-  number
  return $ Op $ M.alter (\case Just x  -> Just (x `o` n)
                               Nothing -> Just (0 `o` n)) r

infxOp :: Parser (Int -> Int -> Int)
infxOp = (string "inc" *> pure (+))
     <|> (string "dec" *> pure (-))

instruction  :: Parser Instr
instruction = do
  o <- op
  _ <- string " if "
  c <- cond
  return $ Instr (\env -> if check c env
                   then run o env
                   else env)

answer1 :: IO Int
answer1 = do
  lines <- lines <$> input
  let instrs  = rights $ (parse instruction "") <$> lines
  let env     = foldl (flip exec) M.empty instrs
  return $ largest env

answer2 :: IO Int
answer2 = do
  lines <- lines <$> input
  let instrs  = rights $ (parse instruction "") <$> lines
  let envs    = scanl' (flip exec) M.empty instrs
  let maxs    = largest <$> envs
  let largest = maximum maxs
  return largest


largest :: Env -> Int
largest map
  | null list = 0
  | otherwise = maximum $ snd <$> list
  where list = M.toList map
