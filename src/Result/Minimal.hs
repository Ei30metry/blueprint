-- |

module Result.Minimal where


import           Data.Coerce
import           Data.Text                 ( Text )
import qualified Data.Text.Lazy.IO         as TLI
import           Data.Tree                 ( Tree (..), drawTree )

import           GHC.Data.FastString       ( lengthFS )
import           GHC.Types.Name.Occurrence ( HasOccName (..),
                                             OccName (occNameFS), occNameString )
import           GHC.Utils.Outputable      ( Outputable (..), SDoc, nest,
                                             pprHsChar, sep, underscore, vbar,
                                             ($+$), (<+>) )
import           GHC.Utils.Ppr             ( text )
import           GHC.Utils.Ppr.Colour

import           Types.AST                 ( BluePrintAST (..) )



-- TODO think about using GHC Outputable instead of prettyprinter
prettyPrintAST :: Bool -> BluePrintAST a -> IO ()
prettyPrintAST ast = undefined

-- TODO try to rewrite pprBAST
-- pprBAST' :: (Outputable a, HasOccName a) => Tree a -> SDoc
-- pprBAST' (Node x []) = ppr x
-- pprBAST' (Node x [t]) = ppr x <+> underscore <+> nest (lengthFS . occNameFS $ occName x) (pprBAST t)
-- pprBAST' (Node x xs) = ppr x <+> underscore <+> nest indentForx (sep (map (($+$ vbar) . pprBAST) xs))
--   where indentForx = lengthFS . occNameFS $ occName x
--         height x = undefined

pprBAST :: forall a. HasOccName a => BluePrintAST a -> String
pprBAST = drawTree . fmap occNameToString . coerce @_ @(Tree a)
  where occNameToString = occNameString . occName
