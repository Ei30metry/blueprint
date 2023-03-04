module Golden5 where

f :: Int -> String
f = g
  where g = show . (+2)


g :: String -> Int
g = read


g' :: String -> Int
g' = g

g'' :: String -> Int
g'' = g'

f' :: Int -> Int
f' = g . f


l :: Int -> Int
l = g . f

test12 :: Int -> String
test12 = show . (+1) . id . f'


f'' :: Int -> String
f'' = f . f1
  where f1 = f'


test1 :: Int -> String
test1 = show . (+1) . id . f'

test1' :: String -> Int
test1' = (+2) . read . test1 . read

map' :: (a -> b) -> [a] -> [b]
map' = map


fPrime :: (Int -> String) -> (String -> Int) -> (Int -> Int)
fPrime f'' g = g . f''


fPrimeWithApplication :: (Int -> Int) -> Int
fPrimeWithApplication t = t 4
