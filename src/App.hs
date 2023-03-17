module App ( runBluePrint
           , bluePrint
           , BluePrint(..)
           ) where

import           Control.Monad.Reader       ( MonadReader )
import           Control.Monad.Trans        ( MonadIO, MonadTrans (..) )
import           Control.Monad.Trans.Except ( ExceptT, runExceptT )
import           Control.Monad.Trans.Reader ( ReaderT (runReaderT), ask )
import           Control.Monad.Trans.Writer ( WriterT (runWriterT) )
import           Control.Monad.Writer       ( MonadWriter )

import           Data.Functor               ( (<&>) )


-- newtype BluePrint a w m b = BT { unBluePrint :: ReaderT a (WriterT w m) b}
--   deriving (Functor, Applicative, Monad, MonadReader a, MonadWriter w, MonadIO)

newtype BluePrint a w m b = BT { unBluePrint :: ReaderT a (WriterT w m) b}
  deriving (Functor, Applicative, Monad, MonadReader a, MonadWriter w, MonadIO)



-- runs the Computation within the BluePrint monad
runBluePrint :: BluePrint p w m a -> p -> m (a, w)
runBluePrint computation env = runWriterT $ runReaderT (unBluePrint computation) env

-- Lifts a function into the BluePrint monad
bluePrint :: (Monoid w, Monad m) => (a -> b) -> BluePrint a w m b
bluePrint f = BT $ f <$> ask
