module Development.Blueprint.Error where

data ASTError = FromPrelude | NonHomePackage deriving Eq


data PipelineError = GhcCouldntRename
                   | EmptySrcFile
                   | CouldntFindModName
                   | FailedCradle
                   | NoCradle deriving Eq

data CompErr


instance Show ASTError where
  show FromPrelude = "You are querying an entity from prelude."
  show NonHomePackage = "You are querying an entity that was not defined in this package."


instance Show PipelineError where
  show GhcCouldntRename = "This should't have happend. GHC couldn't rename the parsed module."
  show EmptySrcFile = "The given source file is empty!"
  show CouldntFindModName = "Couldn't find the module name."
  show FailedCradle = "Couldn't load the hie file."
  show NoCradle = "Blueprint couldn't find an explicit hie.yaml file."
    ++ "Please write one by hand or generate it using the implicit-hie package on hackage."
