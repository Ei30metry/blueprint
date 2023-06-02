-- | The module for working with the Blueprint monad

module Development.Blueprint.Monad.BP (BluePrint
                                      ,runBluePrint
                                      ,bluePrint) where

import           Control.Monad.Trans.Except ( ExceptT, runExceptT )
import           Control.Monad.Trans.Reader ( ReaderT (runReaderT), ask )
import           Control.Monad.Trans.Writer ( WriterT (runWriterT) )



type BluePrint e a w m = ReaderT a (ExceptT e (WriterT w m))

-- runs a Computation within the BluePrint monad
runBluePrint :: BluePrint e r w m a -> r -> m (Either e a, w)
runBluePrint computation env = runWriterT . runExceptT $ runReaderT computation env

-- Lifts a function into the BluePrint monad
bluePrint :: (Monoid w, Monad m) => (a -> b) -> BluePrint e a w m b
bluePrint f = f <$> ask
