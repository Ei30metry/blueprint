{-
This module is the frontend to all the functionalities provided by blueprint and
should be the only module needed in order to make a GUI client for Blueprint
-}

module Blueprint where

import           App                           ( BluePrint (..) )

import           Compute.AST                   ( parseSourceFile,
                                                 rnWithGlobalEnv',
                                                 tcModuleToTcGblEnv,
                                                 valBindsToHsBinds )
import           Compute.Morphisms             ( entityToGlbRdrElt )

import           Control.Monad                 ( (<=<) )
import           Control.Monad.Trans           ( lift )

import           GHC                           ( Backend (NoBackend),
                                                 GhcMonad (..), GhcRn,
                                                 HsGroup (hs_valds),
                                                 LoadHowMuch (..),
                                                 ModSummary (..), Name (..),
                                                 RenamedSource, backend,
                                                 getModSummary, getSession,
                                                 getSessionDynFlags,
                                                 guessTarget, hs_valds, load,
                                                 mkModuleName, parseModule,
                                                 setSessionDynFlags, setTargets,
                                                 typecheckModule, ParsedModule (ParsedModule) )
import           GHC.Tc.Types                  ( TcGblEnv(..) )
import           GHC.Types.Name.Reader         ( GlobalRdrElt )
import           GHC.Types.Name.Set            ( DefUses )

import           Language.Haskell.Syntax.Binds ( HsValBinds )

import           Types                         ( Entity )


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


rnTest :: forall w m. (GhcMonad m, Monoid w) => Entity -> BluePrint ModSummary w m (Maybe GlobalRdrElt)
rnTest ent = BT $ do
  parsed <- unBluePrint parseSourceFile
  (glb,_) <- lift . lift $ rnWithGlobalEnv' parsed
  lift . lift . return $ entityToGlbRdrElt ent glb


seeFromTcGblEnv :: forall w s m. (GhcMonad m, Monoid w) => (TcGblEnv -> s) -> BluePrint ModSummary w m s
seeFromTcGblEnv fieldSelector = BT $ do
  parsed <- unBluePrint parseSourceFile
  lift . lift $ return . fieldSelector . tcModuleToTcGblEnv <=< typecheckModule $ parsed


seeDefUses :: forall w m. (GhcMonad m, Monoid w) => BluePrint ModSummary w m DefUses
seeDefUses = seeFromTcGblEnv tcg_dus
