module CLI (parseSearchEnv) where


import           Control.Applicative         ( (<|>) )

import           Options.Applicative         ( Parser, argument, help, long,
                                               metavar, short, str, strOption,
                                               switch )
import           Options.Applicative.Builder ( command, info, maybeReader,
                                               option, progDesc, subparser,
                                               value )

import           Types                       ( Entity (..), LocalFunc,
                                               OutputType (..), Print (..),
                                               Scope (..), SearchEnv (..),
                                               SearchLevel (..), TypeC (..) )


type Params = SearchEnv

parseFilePath :: Parser FilePath
parseFilePath = argument str (metavar "FILENAME")

-- subparser for the blueprint command: function and type
parseCommand :: Parser Entity
parseCommand = subparser $ funcCommand <> typeCommand
  where funcCommand = command "function" (info parseFuncCommand (progDesc "Get the blueprint of a function."))
        typeCommand = command "type" (info parseTypeCommand (progDesc "Get the blueprint of a data type."))


parseFuncCommand :: Parser Entity
parseFuncCommand = FunctionE <$> (toScope <$> parseFunc <*> parseScope )<*> parseSignature -- FunctionE <$> parseFunc <*> parseScope <*> parseSignature
  where parseSignature = switch (long "type-signatures" <> short 'T' <> help "Get the blueprint of a function and show its type-signature")
        parseFunc = argument str (metavar "FUNCTION NAME")
        parseScope :: Parser (Maybe LocalFunc)
        parseScope = option (maybeReader $ \x -> Just (Just x)) (short 'l' <> long "local-func" <> value Nothing <> help "Get the blueprint of a local function")
        toScope func Nothing          = TopLevel func
        toScope func (Just localFunc) = ParentS func localFunc

-- parses the function command, its options and flags
parseTypeCommand :: Parser Entity
parseTypeCommand =  DataTypeE <$> parseTyConf
  where parseTySynOption = switch (long "type-synonyms" <> short 't' <> help "Show the real types of type synonyms")
        parseTyConf = TypeC <$> argument str (metavar "TYPE NAME")
                                <*> parseTySynOption

-- parses the Level option (number of the levels to go through AST)
parseLevelOption :: Parser SearchLevel
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

-- if not give, the result should be sourcode output
-- IDEA implmenet this with type-level stuff
parseOutputType :: Parser OutputType
parseOutputType = parseSource <|> parsePdf <|> parseImage
  where parseImage = Image <$> strOption (long "image" <> short 'i' <> metavar "FILE" <> help "Write the result to an image")
        parsePdf = PDF <$> strOption (long "pdf" <> short 'p' <> metavar "FILE" <> help "Write the result to a PDF file")
        parseSource = SourceCode <$> option parseF (long "source-code" <> short 's' <> value STDIO <> help "Write the result as a Haskell source to a filePath")
          where parseF = maybeReader $ \x -> Just (File x)

-- parses all the search environment from the command line
-- TODO parse type-signature flag (default is False)
parseSearchEnv :: Parser Params
parseSearchEnv = SEnv <$> parseCommand <*> parseLevelOption <*> parseColorFlag <*> parseLineNumber <*> parseOutputType <*> parseFilePath
