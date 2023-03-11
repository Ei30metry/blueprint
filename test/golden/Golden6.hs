module Golden6 (M.pack) where

import qualified Golden2 as M

f :: Int -> Int
f = (^2)

g :: Int -> String
g = f
  where f = show
