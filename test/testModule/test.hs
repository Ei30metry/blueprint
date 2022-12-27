data Name = Artin | Hoorsa deriving (Show, Eq)

f :: Name -> Int
f Artin  = 0
f Hoorsa = 1


main :: IO ()
main = do
    let result = map f [Artin,Hoorsa]
    print result
