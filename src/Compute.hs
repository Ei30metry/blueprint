module Compute where


import           Control.Applicative       ()
import           Control.Monad             ( mapM, (>=>) )
import           Control.Monad.Extra       ()
import           Control.Monad.Reader      ()
import           Control.Monad.Trans       ( MonadTrans, lift, liftIO )
import           Control.Monad.Trans.Class ()

import           Data.Foldable             ()
import qualified Data.Map                  as M
import           Data.Traversable          ()

import           GHC                       ( GhcRn, HsGroup (..),
                                             RenamedSource )
import           GHC.Driver.Main           ( hscTypecheckRename )
import           GHC.Generics
import           GHC.Types                 ()
import           GHC.Types.Name            ()


-- moduleToAST :: String -> p
-- parseToModuleDecleration :: String -> a
parseToModuleDecleration :: String -> RenamedSource
parseToModuleDecleration = undefined

searchImplementation :: a
searchImplementation = undefined


-- NameSpace == Varname && NameSort == External ....
lookupName :: a
lookupName = undefined

showImplementation :: a
showImplementation = undefined
