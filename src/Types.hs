module Types where


data Entity a = FunctionE a
              | DataTypeE a deriving (Show, Eq)



data SearchEnv = SEnv { entity      :: Entity String
                      , multiSearch :: Bool } deriving (Show, Eq)
