module Types(Entity(..), SearchEnv(..)
            , Scope(..), readScope, SearchLevel(..)
            , TypeC(..)) where

import           Data.List.Extra ( splitOn )


data Entity = FunctionE Scope
            | DataTypeE TypeC deriving (Show, Eq)

data TypeC = TypeC { typeName       :: String
                   , reduceSynonyms :: Bool} deriving (Show, Eq)

type LocalFunc  = String
type ParentFunc = String
type Func       = String


data Scope = TopLevel Func
           | Parent ParentFunc LocalFunc deriving (Eq, Show)


data SearchLevel = Level Int | ToBottom deriving (Show, Eq)

data SearchEnv = SEnv { entity     :: Entity      -- The thing we are searching for
                      , levels     :: SearchLevel -- The levels to go down in AST
                      , withColor  :: Bool        -- Syntax highlighting
                      , lineNumber :: Bool        -- Show line numbers when printing outputs
                      , modPath    :: FilePath }  -- Module path
               deriving (Show, Eq)


readScope :: String -> Maybe Scope
readScope (splitOn ":" -> [p, l]) = Just (Parent p l)
readScope _                       = Nothing
