module App where

import           Control.Monad.Reader       ( MonadReader, Reader, runReader )
import           Control.Monad.Trans.Reader ( ReaderT (runReaderT) )
import           Control.Monad.Trans.Writer ( WriterT (runWriterT) )
import           Control.Monad.Writer       ( MonadWriter )
import Control.Monad.IO.Class (MonadIO)

import           Data.Tree                  ( Forest, Tree )

import           GHC                        ( Backend (..), DynFlags (backend),
                                              GhcMonad (getSession), GhcRn,
                                              GhcT (..), HsValBinds (..),
                                              LoadHowMuch (..),
                                              ParsedModule (ParsedModule),
                                              RenamedSource (..),
                                              SuccessFlag (..),
                                              TypecheckedModule (tm_internals_, tm_renamed_source),
                                              backend, getModSummary,
                                              getSession, getSessionDynFlags,
                                              guessTarget, hs_valds, load,
                                              mkModuleName, parseModule,
                                              runGhcT, setSessionDynFlags,
                                              setTargets, tm_internals_,
                                              tm_renamed_source,
                                              typecheckModule )
import           GHC.Paths                  ( libdir )

import           Types                      ( Entity, SearchEnv )


-- type BluePrint a b = ReaderT a Maybe b
-- type BluePrint a w b = ReaderT a (WriterT a w) b



newtype BluePrint a m w b = BT { unBluePrint :: ReaderT a (WriterT w m) b}
  deriving (Functor, Applicative, Monad, MonadReader a, MonadWriter w, MonadIO)


runBluePrint :: BluePrint a m w b -> a -> m (b, w)
runBluePrint computation env = runWriterT (runReaderT (unBluePrint computation) env)

-- blueprintEnvSearch :: BluePrint SearchEnv w (Tree b)
-- blueprintEnvSearch = undefined


-- runBluePrint :: SearchEnv -> Maybe b
-- runBluePrint = undefined
-- -- runBluePrint = runReaderT (runWriterT blueprintEnvSearch)

mkFileModName :: FilePath -> String
mkFileModName = reverse . takeWhile (/= '/') . reverse . (\fp -> take (length fp - 3) fp)

-- defaultGhcTRunner :: Monad m => GhcT m a -> m a
-- defaultGhcTRunner computation = setUpGhc LoadAllTargets undefined >> computation >>= runGhcT (Just libdir)
