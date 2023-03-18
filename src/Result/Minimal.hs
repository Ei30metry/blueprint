-- |

module Result.Minimal where


import           Data.Coerce
import           Data.Text                 ( Text )
import qualified Data.Text.Lazy.IO         as TLI
import           Data.Tree                 ( Tree (..) )

import           GHC.Data.FastString       ( lengthFS )
import           GHC.Types.Name.Occurrence ( HasOccName (..), OccName (occNameFS) )
import           GHC.Utils.Outputable      ( Outputable (..), SDoc, nest,
                                             pprHsChar, sep, underscore, vbar,
                                             ($+$), (<+>) )
import           GHC.Utils.Ppr             ( text )
import           GHC.Utils.Ppr.Colour

import           Types.AST                 ( BluePrintAST (..) )



-- TODO think about using GHC Outputable instead of prettyprinter
prettyPrintAST :: Bool -> BluePrintAST a -> IO ()
prettyPrintAST ast = undefined

instance (Outputable a, HasOccName a) => Outputable (BluePrintAST a) where
  ppr = pprBAST . coerce @_ @(Tree a)


{-
desired output for `blueprint-exe function "f'" Golden5.hs`

f' _ .
     |
     f _ .
     |   |
     |   +
     |   |
     |   show
     |
     g _ read
-}
pprBAST :: (Outputable a, HasOccName a) => Tree a -> SDoc
pprBAST (Node x []) = ppr x
pprBAST (Node x xs) = ppr x <+> underscore <+> nest indentForx (sep (map (($+$ vbar). pprBAST) xs))
  where indentForx = lengthFS . occNameFS $ occName x
