module Golden5 where

f :: Int -> String
f = g
  where g = show . (+2)


g :: String -> Int
g = read


f' :: Int -> Int
f' = g . f


f'' :: Int -> String
f'' = f . f1
  where f1 = f'
