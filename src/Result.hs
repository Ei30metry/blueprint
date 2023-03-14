module Result ( banner
             , module Result.Pdf
             , module Result.Image
             , module Result.Source
             , bluePrintASTtoTreeString) where

import           Compute.AST          ( BluePrintAST (..) )

import           Data.Coerce          ( coerce )
import           Data.Tree            ( Tree (..), drawTree )

import           GHC                  ( Name )
import           GHC.Utils.Outputable ( Outputable, ppr, showPprUnsafe )

import           Result.Image
import           Result.Pdf
import           Result.Source


banner :: String -> String
banner txt = let prettyBorder = replicate 10 '='
  in mconcat [prettyBorder, " ", txt, " ", prettyBorder, "\n"]


bluePrintASTtoTreeString :: forall a. (Outputable a) => BluePrintAST a -> Tree String
bluePrintASTtoTreeString = fmap (showPprUnsafe . ppr) . coerce @_ @(Tree a)
