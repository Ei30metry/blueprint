module Types.AST ( DataConCantHappen
                 , BluePrintAST(..)) where

import           Data.Functor.Classes ( Show1 )
import           Data.Tree            ( Tree (..) )

import           GHC.Generics         ( Generic )

-- We might need this when using TTG in our own source code
data DataConCantHappen

newtype BluePrintAST a = BAST { unBAST :: Tree a }
  deriving (Applicative, Monad, Functor, Show, Eq, Ord, Show1, Generic)
