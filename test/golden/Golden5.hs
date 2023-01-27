module Golden5 where

data Name = Artin
          | Hoorsa
          | AmirHosseinZ
          | AmirHosseinL
          | Unknown
          | Mehrbod deriving (Show, Eq, Ord, Enum, Bounded)


morphism :: Name -> Name
morphism Artin  = Hoorsa
morphism Hoorsa = Artin
morphism _      = Unknown


f :: Int -> String
f = g
  where g = show . (+2)


g :: String -> Int
g = read


class Loves a where
  loves :: a -> a


instance Loves Name where
  loves = morphism


-- main :: IO ()
-- main = mapM_ (print . loves) ([minBound .. maxBound] :: [Name])
