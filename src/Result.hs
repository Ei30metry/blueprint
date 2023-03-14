module Result ( banner
             , module Result.Pdf
             , module Result.Image
             , module Result.Source
             , bluePrintASTtoTreeString
             , defPrint ) where

import           Compute.AST          ( BluePrintAST (..) )

import           Data.Coerce          ( coerce )
import           Data.Tree            ( Tree (..), drawTree )

import           GHC                  ( Name )
import           GHC.Utils.Outputable ( Outputable(..), showPprUnsafe, SDoc, printSDocLn, defaultSDocContext)
import GHC.Utils.Ppr (Mode (..), style)

import System.IO (stdout)
import           Result.Image
import           Result.Pdf
import           Result.Source


banner :: String -> String
banner txt = let prettyBorder = replicate 10 '='
  in mconcat [prettyBorder, " ", txt, " ", prettyBorder, "\n"]


bluePrintASTtoTreeString :: forall a. Outputable a => BluePrintAST a -> Tree String
bluePrintASTtoTreeString = fmap (showPprUnsafe . ppr) . coerce @_ @(Tree a)


defPrint :: SDoc -> IO ()
defPrint = printSDocLn defaultSDocContext (PageMode True) stdout
