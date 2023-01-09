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

import           GHC.Driver.Main           ()
import           GHC.Generics
import           GHC.Types.Name            ()
import           GHC.Types.Name.Cache      ()
import           GHC.Types.Name.Occurrence ()
import           GHC.Types.Name.Reader     ()
import           GHC.Types.Name.Set        ()


-- moduleToAST :: String -> p
-- parseToModuleDecleration :: String -> Module
parseToModuleDecleration = undefined

searchImplementation :: a
searchImplementation = undefined


getNames :: a
getNames = undefined

showImplementation :: a
showImplementation = undefined
