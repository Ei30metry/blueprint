module CLI where

import           Control.Applicative ( Alternative (..) )

import           Options.Applicative ( Parser, argument, help, long, metavar,
                                       short, str, strOption )

import           Text.Read           ( readMaybe )

import           Types

-- search for local Scope
--local-scope

{-
searching for more than 1 thing
multiple:
  search for more than 1 definition
  search for more than 1 instance
  search for more than 1 data Type
-}

data SearchMode = MultiSearch [Entity]
                | Search Entity deriving (Show, Eq)


data Params = Params SearchMode FilePath deriving (Show, Eq)


-- mkParams :: Parser Params
-- mkParams = Params <$> (search <|> multiSearch) <*> modulePath
--   where search = Search <$> strOption (long "entity" <> short 'e' <> metavar "Entity Name" <> help "The entity you want to search for")
--         multiSearch = MultiSearch <$> many (argument str (long "entities" <> short 'm' <> metavar "Entities" <> help "Search for multiple entities at once"))
--         modulePath = undefined


-- help
printHelp :: a
printHelp = undefined
