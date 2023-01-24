module CLI where

import           Control.Applicative         ( Alternative (..), many, some,
                                               (<**>), (<|>) )

import           Options.Applicative         ( Parser, argument, flag, help,
                                               long, metavar, short, str,
                                               strOption, switch )
import           Options.Applicative.Builder

import           Types                       ( Entity (FunctionE), Scope,
                                               SearchEnv, SearchLevel,
                                               readScope )

data Params = Params SearchEnv FilePath deriving (Show, Eq)


mkParams :: Parser Params
mkParams = Params <$> parseSearchEnv <*> parseFilePath
  where parseSearchEnv :: Parser SearchEnv
        parseSearchEnv = undefined
    -- parseSearchEnv = (parseCommand <|> parseOptsAndFlags) <> parseFilePath

parseFilePath :: Parser FilePath
parseFilePath = undefined

parseCommand :: Parser Entity
parseCommand = subparser $ funcCommand <> typeCommand
  where funcCommand = command "function" (info parseFuncCommand (progDesc "Get the blueprint of a function."))
        typeCommand = command "type" (info parseTypeCommand (progDesc "Get the blueprint of a data type."))



parseFuncCommand :: Parser Entity
parseFuncCommand = FunctionE <$> parseScope
  where parseScopeArgument = maybeReader readScope
        parseScope = option parseScopeArgument (short 's' <> long "local-scope" <> help "search for function in local scope")


parseTypeCommand :: Parser Entity
parseTypeCommand = undefined


parseLevelOption :: Parser SearchLevel
parseLevelOption = undefined


parseColorFlag :: Parser Bool
parseColorFlag = switch (long "color" <> short 'c' <> help "Show with syntax highlighting")

parseLineNumber :: Parser Bool
parseLineNumber = switch (long "line-number" <> short 'n' <> help "Whether or not to show the line numbers")


parseTySynOption :: Parser Bool
parseTySynOption = switch (long "type-synonyms" <> short 't' <> help "Show the real types of type synonyms")
