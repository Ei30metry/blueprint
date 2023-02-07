module Types(Entity(..), SearchEnv(..)
            , Scope(..), SearchLevel(..)
            , TypeC(..), EntityOccDef, ParentFunc, Func, LocalFunc, OutputType(..), Print(..)) where


data Entity = FunctionE Scope Bool
            | DataTypeE TypeC
            deriving (Show, Eq)

-- data Entity = FunctionE Scope Bool
--             | DataTypeE TypeC deriving (show, Eq)

data TypeC = TypeC { typeName       :: String
                   , reduceSynonyms :: Bool   } deriving (Show, Eq)

type LocalFunc     = String
type ParentFunc    = String
type Func          = String
type EntityOccDef  = String


data Scope = TopLevel Func
           | ParentS ParentFunc LocalFunc deriving (Eq, Show)


data SearchLevel = Level Int | ToBottom deriving (Show, Eq)


data Print = STDIO
           | File FilePath
           deriving (Show, Eq)

data OutputType = Image FilePath
                | PDF FilePath
                | SourceCode Print
                deriving (Show, Eq)

-- TODO Add a parameterinorder to determine output type, like pdf or image or source code
data SearchEnv = SEnv { entity     :: Entity      -- The thing we are searching for
                      , levels     :: SearchLevel -- The levels to go down in AST
                      , withColor  :: Bool        -- Syntax highlighting
                      , lineNumber :: Bool        -- Show line numbers when printing outputs
                      , outputType :: OutputType  -- The type of result we expect
                      , modPath    :: FilePath}   -- Path of the Module we are searching in
                deriving (Show, Eq)
