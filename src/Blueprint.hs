{-
This module should be the only module we will need in order to make a
GUI client for Blueprint
-}

module Blueprint where

import App ( BluePrint(..) )

import           Compute

import           Control.Monad                     ( (<=<) )
import           Control.Monad.Trans               ( lift, liftIO, MonadTrans )
import           Control.Monad.Trans.Reader        ( ReaderT, ask, mapReaderT, withReaderT, local )
import           Control.Monad.Trans.Writer.Lazy   ( WriterT )

import           GHC                               ( Backend (NoBackend),
                                                     GhcMonad (..),
                                                     GhcPass (GhcRn), GhcRn,
                                                     HsGroup (hs_valds),
                                                     LoadHowMuch (..),
                                                     ModSummary (..), Name (..),
                                                     ParsedModule (ParsedModule),
                                                     RenamedSource, backend,
                                                     getModSummary, getSession,
                                                     getSessionDynFlags,
                                                     guessTarget, hs_valds,
                                                     load, mkModuleName,
                                                     parseModule,
                                                     setSessionDynFlags,
                                                     setTargets,
                                                     typecheckModule )
import           GHC.Types.Name.Reader             ( GlobalRdrEnv(..), GlobalRdrElt )
import           GHC.Utils.Panic                   ( panic )

import           Language.Haskell.Syntax.Binds     ( HsValBinds )
import           Language.Haskell.Syntax.Extension ( IdP )
import Types


mkFileModName :: FilePath -> String
mkFileModName = reverse . takeWhile (/= '/') . reverse . (\fp -> take (length fp - 3) fp)


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


rnSrcToBinds' :: GhcMonad m => RenamedSource -> m (HsValBinds GhcRn)
rnSrcToBinds' = return . hs_valds . \(b,_,_,_) -> b

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
prototypeFunc = return . valBindsToHsBinds <=< rnSrcToBinds' <=< return . snd <=< rnWithGlobalEnv' <=< parseSourceFile' LoadAllTargets


-- rnTest :: forall w m. (Monad m, Monoid w, GhcMonad m) => Entity -> BluePrint ModSummary w m [GlobalRdrElt]
-- rnTest :: forall w m. (Monad m, Monoid w, GhcMonad m) => Entity -> BluePrint ModSummary w m [GlobalRdrEnv]
-- rnTest :: BluePrint ModSummary w m [GlobalRdrEnv]
rnTest :: forall w m. (GhcMonad m, Monoid w) => Entity -> BluePrint ModSummary w m [GlobalRdrElt]
rnTest ent = BT $ do
  parsed <- unBluePrint parseSourceFile
  (glb,_) <- lift . lift $ rnWithGlobalEnv' parsed
  lift . lift . return $ entityToGlbRdrElt ent glb
-- rnTest ent = do
--   parsed <- parseSourceFile
--   (glb,_) <- lift $ rnWithGlobalEnv' parsed
--   let x = lift . lift $ entityToGlbRdrElt ent glb
--   x
