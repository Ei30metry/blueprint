module Golden1 where
-- {-# LANGUAGE DataKinds              #-}
-- {-# LANGUAGE GADTs                  #-}
-- {-# LANGUAGE OverloadedLabels       #-}
-- {-# LANGUAGE OverloadedStrings      #-}
-- {-# LANGUAGE ScopedTypeVariables    #-}
-- {-# LANGUAGE TypeFamilies           #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}

-- import           Data.Kind ( Constraint, Type )



-- data Exp (a :: Type) where
--   LitInt :: Int -> Exp Int
--   LitBool :: Bool -> Exp Bool
--   Mult :: Exp Int -> Exp Int -> Exp Int
--   Plus :: Exp Int -> Exp Int -> Exp Int
--   Minus :: Exp Int -> Exp Int  -> Exp Int
--   Equal :: Exp Int -> Exp Int -> Exp Bool


-- elimExp :: forall a. (Num a, Eq a) => Exp a -> a
-- elimExp (LitInt a)  = a
-- elimExp (LitBool a) = a
-- elimExp (Mult a b)  = elimExp a * elimExp b
-- elimExp (Plus a b)  = elimExp a + elimExp b
-- elimExp (Minus a b) = elimExp a - elimExp b
-- elimExp (Equal a b) = elimExp a == elimExp b


-- type family Foo a b :: Constraint where
--   Foo a b = a ~ b


f :: Int -> Int
f x = 3*x + 2

g :: String -> Int
g = (+2) . read


myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

-- a = Lit 2
-- b = Lit 3
-- c = Mult a b
-- d = Minus c a
