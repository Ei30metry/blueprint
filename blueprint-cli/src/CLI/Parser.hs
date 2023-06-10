-- | Command line parser

module CLI.Parser where

import           Control.Applicative         ( (<|>), many )

import           Data.Text                   ( pack )

import           Development.Blueprint.Types ( DepthLevel (..), Entity (..),
                                               EntityName, Func(..),
                                               LocalFunc, OutputType (..),
                                               Print (..), Scope (..),
                                               SearchEnv (..), TypeC (..),
                                               VisualView (..) )

import           Options.Applicative         ( Parser, ReadM, argument, flag,
                                               help, long, metavar, optional,
                                               readerError, short, str,
                                               strOption, switch )
import           Options.Applicative.Builder ( auto, command, info, maybeReader,
                                               option, progDesc, subparser,
                                               value )


type Params = SearchEnv

parseFilePath :: Parser FilePath
parseFilePath = argument str (metavar "FILENAME")

-- subparser for the blueprint command: function and type
parseCommand :: Parser Entity
parseCommand = subparser $ subCommand <> showCommand
  where subCommand = command "substitude" (info parseSubCommand (progDesc "Get the blueprint of a function or type with term substitution"))
        showCommand = command "show" (info parseShowCommand (progDesc "Show the blueprint of a function or type in given format"))

parseSubCommand :: Parser Entity
parseSubCommand = SubstituteE <$> parseEntityName <*> many parseEntityName

parseEntityName :: Parser EntityName
parseEntityName = argument str (metavar "NAME")

parseShowCommand :: Parser Entity
parseShowCommand = ShowE <$> parseEntityName

-- parses the Level option (number of the levels to go through AST)
parseLevelOption :: Parser DepthLevel
parseLevelOption = option pLevel (long "level" <> short 'L' <> metavar "NUMBER" <> value ToBottom <> help "Levels of implementation search in AST")
  where pLevel = maybeReader $ \x -> Just (Level $ read x)
  -- where pLevel = maybeReader . Just . Level . read


-- parses the color flag
parseColorFlag :: Parser Bool
parseColorFlag = switch (long "color" <> short 'c'
                      <> help "Show with syntax highlighting")

-- parses the line number flag
parseLineNumber :: Parser Bool
parseLineNumber = switch (long "line-number" <> short 'n'
                       <> help "show the line numbers")

-- parses the output type. as of version 1, the default output would be Minimal
parseOutputType :: Parser OutputType
parseOutputType = parseAscii <|> parseImage <|> parseSource
  where parseImage = Image <$> option parseV (long "image" <> short 'i' <> metavar "FILE" <> value Browser <> help "Write the result to an image")
        parseAscii = Ascii <$> option parseF (long "minamal" <> short 'm' <> value STDIO <> help "Prints a minimal result to Stdio or a filepath")
        parseSource = SourceCode <$> option parseF (long "source-code" <> short 's' <> value STDIO <> help "Write the result as a Haskell source to a filePath")
        parseF = maybeReader $ \x -> Just (File x)
        parseV = maybeReader $ \x -> Just (DumpFile x)

-- parses all the search environment from the command line
-- TODO parse type-signature flag (default is False)
parseSearchEnv :: Parser Params
parseSearchEnv = SEnv <$> parseCommand <*> parseLevelOption <*> parseColorFlag <*> parseLineNumber <*> parseOutputType <*> parseFilePath
