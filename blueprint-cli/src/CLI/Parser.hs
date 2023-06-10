-- | Command line parser

module CLI.Parser where

import           Control.Applicative         ( (<|>) )

import           Data.Text                   ( pack )

import           Development.Blueprint.Types ( DepthLevel (..), Entity (..),
                                               EntityName, Func(..),
                                               LocalFunc, OutputType (..),
                                               OutputFormat(..),
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
parseCommand = subparser $ funcCommand <> typeCommand <> subCommand <> showCommand
  where funcCommand = command "function" (info parseFuncCommand (progDesc "Get the blueprint of a function."))
        typeCommand = command "type" (info parseTypeCommand (progDesc "Get the blueprint of a data type."))
        subCommand = command "substitude" (info parseSubCommand (progDesc "Get the blueprint of a function or type with term substitution."))
        showCommand = command "show" (info parseShowCommand (progDesc "Show the blueprint of a function or type ingiven format: 'ascii', 'img'"))

parseFuncCommand :: Parser Entity
parseFuncCommand = FunctionE <$> (toScope <$> parseFunc <*> parseScope ) <*> parseSignature -- FunctionE <$> parseFunc <*> parseScope <*> parseSignature
  where parseSignature = switch (long "type-signatures" <> short 'T' <> help "Get the blueprint of a function and show its type-signature")
        parseFunc :: Parser Func
        parseFunc = argument str (metavar "FUNCTION NAME")
        parseScope :: Parser (Maybe LocalFunc)
        parseScope = option (maybeReader $ \x -> Just (Just (pack x))) (short 'l' <> long "local-func" <> value Nothing <> help "Get the blueprint of a local function")
        toScope func = maybe (TopLevel func) (ParentS func)

-- parses the function command, its options and flags
parseTypeCommand :: Parser Entity
parseTypeCommand =  DataTypeE <$> parseTyConf
  where parseTySynOption = switch (long "type-synonyms" <> short 't' <> help "Show the real types of type synonyms")
        parseTyConf = TypeC <$> argument str (metavar "TYPE NAME")
                                <*> parseTySynOption

parseSubCommand :: Parser Entity
parseSubCommand = SubstituteE <$> parseEntityName <*> parseSubLevel
  where parseSubLevel = optional $ option auto (long "max-level" <> short 'L' <> help "Maximum level of substitution")

parseEntityName :: Parser EntityName
parseEntityName = argument str (metavar "NAME")

parseShowCommand :: Parser Entity
parseShowCommand = ShowE <$> parseEntityName <*> parseFormat
  where
    parseFormat = option parseOutputFmt (long "output-format" <> short 'o' <>
      help "Output format. Can be 'ascii', 'img'")
    parseOutputFmt :: ReadM OutputFormat
    parseOutputFmt = str @String >>= \case
      "ascii" -> pure FmtAscii
      "img" -> pure FmtImg
      _ -> readerError "Unsupported output format. Use 'ascii' or 'img'"

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
parseOutputType = parseMinimal <|> parseImage <|> parseSource <|> parseJSON
  where parseImage = Image <$> option parseV (long "image" <> short 'i' <> metavar "FILE" <> value Browser <> help "Write the result to an image")
        parseJSON = JSONOutput <$> option parseF (long "json" <> short 'j' <> metavar "FILE" <> value STDIO <> help "Write the result as a json file")
        parseMinimal = Minimal <$> option parseF (long "minamal" <> short 'm' <> value STDIO <> help "Prints a minimal result to Stdio or a filepath")
        parseSource = SourceCode <$> option parseF (long "source-code" <> short 's' <> value STDIO <> help "Write the result as a Haskell source to a filePath")
        parseF = maybeReader $ \x -> Just (File x)
        parseV = maybeReader $ \x -> Just (DumpFile x)

-- parses all the search environment from the command line
-- TODO parse type-signature flag (default is False)
parseSearchEnv :: Parser Params
parseSearchEnv = SEnv <$> parseCommand <*> parseLevelOption <*> parseColorFlag <*> parseLineNumber <*> parseOutputType <*> parseFilePath
