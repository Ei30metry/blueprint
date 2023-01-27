module Types(Entity(..), SearchEnv(..)
            , Scope(..), SearchLevel(..)
            , TypeC(..), EntityOccDef, ParentFunc, Func, LocalFunc) where


data Entity = FunctionE Scope
            | DataTypeE TypeC deriving (Show, Eq)

data TypeC = TypeC { typeName       :: String
                   , reduceSynonyms :: Bool} deriving (Show, Eq)

type LocalFunc     = String
type ParentFunc    = String
type Func          = String
type EntityOccDef = String


data Scope = TopLevel Func
           | ParentS ParentFunc LocalFunc deriving (Eq, Show)


data SearchLevel = Level Int | ToBottom deriving (Show, Eq)

data SearchEnv = SEnv { entity     :: Entity      -- The thing we are searching for
                      , levels     :: SearchLevel -- The levels to go down in AST
                      , withColor  :: Bool        -- Syntax highlighting
                      , lineNumber :: Bool        -- Show line numbers when printing outputs
                      , modPath    :: FilePath }  -- Module path
               deriving (Show, Eq)
