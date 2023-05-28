{-
This module is the frontend to all the functionalities provided by blueprint and
should be the only module needed in order to make a GUI client for Blueprint
-}

-- TODO fix this
module Development.Blueprint where

import           Control.Monad.Reader                    ( MonadIO (liftIO),
                                                           MonadTrans (lift),
                                                           (<=<) )
import           Control.Monad.Trans.Except              ( ExceptT, except,
                                                           throwE )

import           Development.Blueprint.App               ( BluePrint (..) )
import           Development.Blueprint.Compute.AST       ( parseSourceFile,
                                                           tcModuleToTcGblEnv,
                                                           valBindsToHsBinds )
import           Development.Blueprint.Compute.Morphisms ( entityToGlbRdrElt )
import           Development.Blueprint.Error             ( PipelineError(..) )
import           Development.Blueprint.Types             ( Entity )

import           GHC                                     ( Backend (..),
                                                           GhcPass (GhcRn),
                                                           GhcRn,
                                                           HsGroup (hs_valds),
                                                           LoadHowMuch (..),
                                                           ModSummary (..),
                                                           Name (..),
                                                           ParsedModule (..),
                                                           RenamedSource,
                                                           backend,
                                                           getModSummary,
                                                           getSessionDynFlags,
                                                           guessTarget,
                                                           hs_valds, load,
                                                           mkModuleName,
                                                           parseModule,
                                                           setSessionDynFlags,
                                                           setTargets,
                                                           typecheckModule )
import           GHC.Driver.Monad                        ( GhcMonad (..),
                                                           getSessionDynFlags )
import           GHC.Driver.Session                      ( DynFlags (backend, ghcLink, ghcMode),
                                                           GhcLink (LinkInMemory),
                                                           GhcMode (CompManager) )
import           GHC.Tc.Types                            ( TcGblEnv (..) )
import           GHC.Types.Name.Reader                   ( GlobalRdrElt )
import           GHC.Types.Name.Set                      ( DefUses )

import           HIE.Bios                                ( CradleLoadResult (CradleSuccess),
                                                           findCradle,
                                                           getCompilerOptions,
                                                           loadCradle )
import           HIE.Bios.Environment                    ( initSession )

import           System.Directory                        ( getCurrentDirectory )


-- FIXME terrible performance
-- findModuleName :: String -> Either String String
findModuleName :: Monad m => String -> ExceptT PipelineError m String
findModuleName = nonMainModule . dropWhile (/= "module") . words
  where nonMainModule []      = throwE EmptySrcFile
        nonMainModule (_:x:_) = return x
        nonMainModule _       = throwE CouldntFindModName


-- | Sets up the right environment for ghc to compute on
-- TODO use mtl
initializeGhc :: (GhcMonad m) => FilePath -> ExceptT PipelineError m ModSummary
initializeGhc filePath = do
    fileContent <- liftIO $ readFile filePath
    fileModuleName <- findModuleName fileContent
    dir <- liftIO getCurrentDirectory
    maybeCradle <- liftIO $ findCradle (dir <> "/")
    mPath <- convertCradle maybeCradle
    p <- liftIO $ return mPath
    cradle <- liftIO (loadCradle p)
    comp <- liftIO $ getCompilerOptions filePath cradle
    setupOptions (fileModuleName, comp)

  where
    convertCradle (Just x) = return x
    convertCradle Nothing  = throwE NoCradle

    setupOptions (modName , CradleSuccess r) = do
            dflags <- lift $ initSession r >> getSessionDynFlags
            lift . setSessionDynFlags $ dflags { backend = NoBackend, ghcLink = LinkInMemory, ghcMode = CompManager }
            target <- lift $ guessTarget filePath Nothing
            lift $ setTargets [target] >> load LoadAllTargets
            lift $ getModSummary $ mkModuleName modName

    setupOptions _ = throwE FailedCradle


-- TODO use mtl
seeFromTcGblEnv :: forall w s m e. (GhcMonad m, Monoid w) => (TcGblEnv -> s) -> BluePrint e ModSummary w m s
seeFromTcGblEnv fieldSelector = BT $ do
  parsed <- unBluePrint parseSourceFile
  lift . lift . lift $ return . fieldSelector . tcModuleToTcGblEnv <=< typecheckModule $ parsed


seeDefUses :: forall w m e. (GhcMonad m, Monoid w) => BluePrint e ModSummary w m DefUses
seeDefUses = seeFromTcGblEnv tcg_dus
