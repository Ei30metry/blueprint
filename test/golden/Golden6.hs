module Golden6 where

f :: Int -> Int
f = (^2)

g :: Int -> String
g = f
  where f = show
