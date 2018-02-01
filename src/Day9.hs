module Day9 where


import           Data.Monoid          (Sum (..))
import           Data.Void
import           Text.Megaparsec      hiding (Stream)
import           Text.Megaparsec.Char

type Parser = Parsec Void String

data Group
  = Group [Group]
  | Garbage [GarbageContent]
  | Content String
  | Negation Char
  deriving (Show)

data GarbageContent
  = Real Char
  | Neg Char
  deriving (Show)

input :: IO String
input = readFile "input/day9-input.txt"

answer1 = do
  g <- parse group "" <$> input
  return $ score <$> g

answer2 = do
  g <- parse group "" <$> input
  return $ filteredGarbage <$> g

score :: Group -> Int
score g = go 1 g
  where
    go :: Int -> Group -> Int
    go d (Garbage _) = 0
    go d (Content _) = 0
    go d (Group gs) =
      d + (getSum $ foldMap (Sum . go (succ d)) gs)

filteredGarbage :: Group -> Int
filteredGarbage (Group gs)  = getSum $ foldMap (Sum . filteredGarbage) gs
filteredGarbage (Garbage x) = length $ filter realGarbage x
  where realGarbage (Real x) = True
        realGarbage (Neg  x) = False
filteredGarbage (Content _) = 0

negation :: Parser Char
negation = char '!'
        *> anyChar

group :: Parser Group
group = choice [ Negation <$> negation *> group
               , Group    <$> between (char '{') (char '}') (group `sepBy` char ',')
               , Garbage  <$> between (char '<') (char '>') (many (Neg <$> negation <|> Real <$> noneOf ['>']))
               , Content  <$> many letterChar
               ]


-- 15089 too high
