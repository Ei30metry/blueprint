module Development.Blueprint.App ( runBluePrint
           , bluePrint
           , BluePrint(..)
           ) where

import           Control.Monad.Trans.Except ( ExceptT, runExceptT )
import           Control.Monad.Trans.Reader ( ReaderT (runReaderT), ask )
import           Control.Monad.Trans.Writer ( WriterT (runWriterT) )



-- newtype BluePrint a w m b = BT { unBluePrint :: ReaderT a (WriterT w m) b}
--   deriving (Functor, Applicative, Monad, MonadReader a, MonadWriter w, MonadIO)


-- TODO change the position of e
newtype BluePrint e a w m b = BT { unBluePrint :: ReaderT a (ExceptT e (WriterT w m)) b}

-- runs a Computation within the BluePrint monad
runBluePrint :: BluePrint e r w m a -> r -> m (Either e a, w)
runBluePrint computation env = runWriterT . runExceptT $ runReaderT (unBluePrint computation) env

-- Lifts a function into the BluePrint monad
bluePrint :: (Monoid w, Monad m) => (a -> b) -> BluePrint e a w m b
bluePrint f = BT $ f <$> ask
