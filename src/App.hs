module App where

import           Control.Monad.Reader       ( Reader, runReader )
import           Control.Monad.Trans.Reader ( ReaderT (runReaderT) )
import           Control.Monad.Trans.Writer ( WriterT (runWriterT) )

import           Data.Tree                  ( Forest, Tree )

import           Types                      ( Entity, SearchEnv )


-- type BluePrint a b = ReaderT a Maybe b
type BluePrint a w b = ReaderT a (WriterT a w) b

-- >>> :t WriterT


blueprintEnvSearch :: BluePrint SearchEnv w (Tree b)
blueprintEnvSearch = undefined


runBluePrint :: SearchEnv -> Maybe b
runBluePrint = runReaderT (runWriterT blueprintEnvSearch)
