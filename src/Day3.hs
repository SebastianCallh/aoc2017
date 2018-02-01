module Day3 where

-- Returns the end of the lap of provided index
end :: Num a => a -> a
end i = (2*i + 1)^2

distance :: Int -> Int
distance 1 = 0
distance x = l + axisDist l x
  where l = lapDist x

axisDist :: Integral r => r -> r -> r
axisDist j i = abs $ f - j
  where
    f = n `rem` (2*j)                 -- Index in ring, cycled
    n = i - (end . pred $ lapDist i)  -- Index in ring

-- Returns the index of the lap of provided tile
lapDist :: (Enum a, Num a, Ord a) => a -> a
lapDist tile = head [i | i <- [1..], tile <= end i]



answer1 = distance input
  where input = 475

answer2 = undefined















-- Returns the corners of provided lap index
corners :: (Integral t, RealFrac r) => r -> [t]
corners i = [e, e - d, e - 2*d, e - 3*d]
  where d = floor $ 2*i -- Distance of one edge
        e = floor $ end i
