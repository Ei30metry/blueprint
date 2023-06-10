module Development.Blueprint.Types( Entity(..), EntityName, SearchEnv(..)
            , Scope(..), DepthLevel(..)
            , TypeC(..), EntityOccDef
            , ParentFunc, Func
            , LocalFunc, OutputType(..)
            , Print(..), VisualView(..)) where

import           Data.Text ( Text )

data Entity = SubstituteE EntityName [EntityName]
            | ShowE EntityName
            deriving (Show, Eq)

data TypeC = TypeC { typeName       :: Text
                   , reduceSynonyms :: Bool   } deriving (Show, Eq)

type LocalFunc     = Text
type ParentFunc    = Text
type Func          = Text
type EntityOccDef  = Text
type EntityName    = Text

data Scope = TopLevel Func
           | ParentS ParentFunc LocalFunc deriving (Eq, Show)


data DepthLevel = Level Int | ToBottom deriving (Show, Eq)


data Print = STDIO
           | File FilePath
           deriving (Show, Eq)

data VisualView = Browser
                | DumpFile FilePath deriving (Show, Eq)

data OutputType = Image VisualView -- SVG view
                | SourceCode Print -- Haskell source code
                | Ascii Print    -- like UNIX tree command
                deriving (Show, Eq)

data SearchEnv = SEnv { entity     :: Entity      -- The thing we are searching for
                      , levels     :: DepthLevel  -- The levels to go down in AST
                      , withColor  :: Bool        -- Syntax highlighting
                      , lineNumber :: Bool        -- Show line numbers when printing outputs
                      , outputType :: OutputType  -- The type of result we expect
                      , modPath    :: FilePath }   -- Path of the Module we are searching in
                deriving (Show, Eq)
