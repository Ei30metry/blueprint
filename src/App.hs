module App where

import           Control.Monad.Reader       ( Reader, runReader )
import           Control.Monad.Trans.Reader ( ReaderT (runReaderT) )
import           Control.Monad.Trans.Writer ( WriterT (runWriterT) )

import           Types                      ( Entity, SearchEnv )


type BluePrint a b = ReaderT a Maybe b


blueprintEnvSearch :: BluePrint SearchEnv b
blueprintEnvSearch = undefined


runBluePrint :: SearchEnv -> Maybe b
runBluePrint = runReaderT blueprintEnvSearch
