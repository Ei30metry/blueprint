module Blueprint where

import           App

import           Compute

import           Control.Monad                     ( (<=<) )
import           Control.Monad.Trans               ( lift )
import           Control.Monad.Trans.Reader        ( ReaderT, ask )
import           Control.Monad.Trans.Writer.Lazy   ( WriterT )

import           GHC                               ( Backend (NoBackend),
                                                     GhcMonad (..),
                                                     GhcPass (GhcRn),
                                                     GhcRn,
                                                     LoadHowMuch (..),
                                                     ModSummary (..), Name (..),
                                                     ParsedModule (ParsedModule),
                                                     backend, getModSummary,
                                                     getSession,
                                                     getSessionDynFlags,
                                                     guessTarget, load,
                                                     mkModuleName, parseModule,
                                                     setSessionDynFlags,
                                                     setTargets, RenamedSource, hs_valds, HsGroup (hs_valds) )
import           GHC.Utils.Panic                   ( panic )

import           Language.Haskell.Syntax.Extension ( IdP )
import Language.Haskell.Syntax.Binds (HsValBinds)
{-
This module should be the only module we will need in order to make a
GUI client for Blueprint
-}

mkFileModName :: FilePath -> String
mkFileModName = reverse . takeWhile (/= '/') . reverse . (\fp -> take (length fp - 3) fp)


rnSrcToBinds' :: GhcMonad m => RenamedSource -> m (HsValBinds GhcRn)
rnSrcToBinds' = return . hs_valds . \(b,_,_,_) -> b


initializeGhc :: GhcMonad m => FilePath -> m ModSummary
initializeGhc filePath = do
  let fileModuleName = mkFileModName filePath
  getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { backend = NoBackend }
  target <- guessTarget filePath Nothing
  setTargets [target]
  load LoadAllTargets -- TODO construct a Unit in order to feed your path to your module
  getModSummary $ mkModuleName fileModuleName


parseSourceFile' :: GhcMonad m => LoadHowMuch -> FilePath -> m ParsedModule
parseSourceFile' loadHowMuch filePath = do
  let fileModuleName = mkFileModName filePath
  getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { backend = NoBackend }
  target <- guessTarget filePath Nothing
  setTargets [target]
  load loadHowMuch -- TODO construct a Unit in order to feed your path to your module
  modSum <- getModSummary $ mkModuleName fileModuleName
  parseModule modSum



prototypeFunc :: GhcMonad m => FilePath -> m [Name]
prototypeFunc = return . valBindsToHsBinds <=< rnSrcToBinds' <=< handleRenamed . snd <=< rnWithGlobalEnv' <=< parseSourceFile' LoadAllTargets
  where handleRenamed (Just x) = return x
        handleRenamed Nothing  = panic "Couldln't run our parsed module through Renamer"


prototypeFuncB' :: GhcMonad m => ReaderT FilePath (WriterT String m) [Name]
prototypeFuncB' = ask >>= \filePath -> lift . lift $ prototypeFunc filePath
