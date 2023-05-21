-- | Computation monad, seperated from the BluePrint monad

module Types.Compute where

import           Control.Monad.ST
import           Control.Monad.Trans.Except ( ExceptT, runExceptT )
import           Control.Monad.Trans.State

import           Data.STRef


-- integrate the ST monad
-- newtype Computation s a e b = Compute { unComputation :: (ExceptT e (StateT s)) b }
