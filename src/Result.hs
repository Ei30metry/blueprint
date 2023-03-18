module Result ( banner
             , module Result.HTML
             , module Result.Minimal
             , module Result.JSON
             , module Result.Image
             , module Result.Source
             , bluePrintASTtoTreeString
             , bluePrintASTtoTreeString'
             , defPrint ) where

import           Types.AST          ( BluePrintAST (..) )

import           Data.Coerce          ( coerce )
import           Data.Text            ( Text, pack )
import qualified Data.Text            as T
import           Data.Tree            ( Tree (..), drawTree )

import           GHC.Utils.Outputable ( Outputable (..), SDoc,
                                        defaultSDocContext, printSDocLn,
                                        showPprUnsafe )
import           GHC.Utils.Ppr        ( Mode (..) )

import           Result.HTML
import           Result.Image
import           Result.JSON
import           Result.Minimal
import           Result.Source

import           System.IO            ( stdout )


banner :: Text -> Text
banner txt = let prettyBorder = T.replicate 10 "="
  in mconcat [prettyBorder, " ", txt, " ", prettyBorder, "\n"]


bluePrintASTtoTreeString :: forall a. Outputable a => BluePrintAST a -> Tree Text
bluePrintASTtoTreeString = fmap (pack . showPprUnsafe . ppr) . coerce @_ @(Tree a)


bluePrintASTtoTreeString' :: forall a. Outputable a => BluePrintAST a -> Tree String
bluePrintASTtoTreeString' = fmap (showPprUnsafe . ppr) . coerce @_ @(Tree a)

defPrint :: SDoc -> IO ()
defPrint = printSDocLn defaultSDocContext (PageMode True) stdout
