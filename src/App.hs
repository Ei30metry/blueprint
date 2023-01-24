module App where

import           Control.Monad.Trans.Reader ( ReaderT (runReaderT) )
import           Control.Monad.Trans.Writer ( WriterT (runWriterT) )

import           Types                      ( Entity, SearchEnv )


type BluePrint a b = ReaderT a Maybe b


runBluePrint :: BluePrint a b -> Maybe b
runBluePrint = undefined
