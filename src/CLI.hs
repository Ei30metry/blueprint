module CLI where

import           Control.Applicative         ( Alternative (..), many, some,
                                               (<**>), (<|>) )

import           Options.Applicative         ( Parser, argument, flag, help,
                                               long, metavar, short, str,
                                               strOption, switch )
import           Options.Applicative.Builder

import           Types                       ( Entity (..), Scope (..),
                                               SearchEnv (..), SearchLevel (..),
                                               TypeC (..), readScope )


type Params = SearchEnv

parseFilePath :: Parser FilePath
parseFilePath = argument str (metavar "FILENAME")

-- subparser for the blueprint command: function and type
parseCommand :: Parser Entity
parseCommand = subparser $ funcCommand <> typeCommand
  where funcCommand = command "function" (info parseFuncCommand (progDesc "Get the blueprint of a function."))
        typeCommand = command "type" (info parseTypeCommand (progDesc "Get the blueprint of a data type."))


-- parses the function command and, its options and its flags
parseFuncCommand :: Parser Entity
parseFuncCommand = FunctionE <$> ((TopLevel <$> parseFunction) <|> parseScopedFunction)
  where parseFunction = argument str (metavar "FUNCTION NAME")
        parseScopedFunction = Parent <$> parseFunction <*> option str (long "scope" <> short 's' <> help "Get the blueprint of a local function")


-- parses the function command, its options and flags
parseTypeCommand :: Parser Entity
parseTypeCommand =  DataTypeE <$> parseTyConf
  where parseTySynOption = switch (long "type-synonyms" <> short 't' <> help "Show the real types of type synonyms")
        parseTyConf = TypeC <$> argument str (metavar "TYPE NAME")
                                <*> parseTySynOption

-- parses the Level option (number of the levels to go through AST)
parseLevelOption :: Parser SearchLevel
parseLevelOption = option pLevel (long "level" <> short 'l' <> metavar "NUMBER" <> value ToBottom <> help "Levels of implementation search in AST")
  where pLevel = maybeReader $ \x -> Just (Level $ read x)


-- parses the color flag
parseColorFlag :: Parser Bool
parseColorFlag = switch (long "color" <> short 'c'
                      <> help "Show with syntax highlighting")

-- parses the line number flag
parseLineNumber :: Parser Bool
parseLineNumber = switch (long "line-number" <> short 'n'
                       <> help "show the line numbers")


-- parses all the search environment from the command line
parseSearchEnv :: Parser Params
parseSearchEnv = SEnv <$> parseCommand <*> parseLevelOption <*> parseColorFlag <*> parseLineNumber <*> parseFilePath
