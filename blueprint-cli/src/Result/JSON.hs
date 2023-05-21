module Result.JSON where

-- import           Types.AST                   ( BluePrintAST (..) )

-- import           Data.Aeson                    ( ToJSON (..), Value )
-- import           Data.Aeson.Encode.Pretty      ( Config (confIndent),
--                                                  Indent (Spaces), defConfig,
--                                                  encodePretty' )
-- import           Data.ByteString.Lazy.Internal ( ByteString )
-- import           Data.Coerce                   ( coerce )
-- import           Data.Text                     ( pack )
-- import           Data.Tree                     ( Tree (..) )

-- import           GHC.Types.Name                ( Name, nameOccName,
--                                                  occNameString )

-- instance Show Name where
--   show = occNameString . nameOccName

-- -- Pretty print the resulting AST in JSON format
-- prettyPrintJSON :: forall a. Show a => BluePrintAST a -> ByteString
-- prettyPrintJSON = encodePretty' conf . fmap (pack . show) . coerce @_ @(Tree a)
--   where conf = defConfig {confIndent = Spaces 2}


-- prettyPrintJSON' :: forall a. Show a => BluePrintAST a -> Value
-- prettyPrintJSON' = toJSON . fmap (pack . show) . coerce @_ @(Tree a)
