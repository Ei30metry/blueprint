module Types where

import           Control.Lens



data Entity = InstanceE
            | FunctionE
            | DataTypeE deriving (Show, Eq)



newtype MultiSearch = MultiI [Entity] deriving (Show, Eq)


data LocalScope (a :: Entity) where
  LclScopeFunc :: LocalScope 'FunctionE


data SearchEnv = SEnv { entity      :: Entity
                      , localScope  :: Bool
                      , multiSearch :: Bool } deriving (Show, Eq)


convertLocalScope :: LocalScope a -> Entity
convertLocalScope LclScopeFunc = FunctionE
