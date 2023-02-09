module App where

import           Control.Monad.Reader       ( Reader, runReader )
import           Control.Monad.Trans.Reader ( ReaderT (runReaderT) )
import           Control.Monad.Trans.Writer ( WriterT (runWriterT) )

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
type BluePrint a w b = ReaderT a (WriterT a w) b

-- >>> :t WriterT


blueprintEnvSearch :: BluePrint SearchEnv w (Tree b)
blueprintEnvSearch = undefined


runBluePrint :: SearchEnv -> Maybe b
runBluePrint = undefined
-- runBluePrint = runReaderT (runWriterT blueprintEnvSearch)

mkFileModName :: FilePath -> String
mkFileModName = reverse . takeWhile (/= '/') . reverse . (\fp -> take (length fp - 3) fp)

setUpGhc :: GhcMonad m => LoadHowMuch -> FilePath -> m SuccessFlag
setUpGhc loadHowMuch filePath = do
  let fileModuleName = mkFileModName filePath
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { backend = NoBackend }
  target <- guessTarget filePath Nothing
  setTargets [target]
  load loadHowMuch -- TODO construct a Unit in order to feed your path to your module


-- defaultGhcTRunner :: Monad m => GhcT m a -> m a
-- defaultGhcTRunner computation = setUpGhc LoadAllTargets undefined >> computation >>= runGhcT (Just libdir)
