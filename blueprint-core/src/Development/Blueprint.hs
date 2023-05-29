{-
This module is the frontend to all the functionalities provided by blueprint and
should be the only module needed in order to make a GUI client for Blueprint
-}

module Development.Blueprint where

import           Control.Monad.Except                    ( MonadError (..) )
import           Control.Monad.Reader                    ( MonadIO (liftIO),
                                                           MonadReader,
                                                           MonadTrans (lift),
                                                           (<=<) )
import           Control.Monad.Trans.Except              ( ExceptT, except,
                                                           throwE )

import           Development.Blueprint.App               ( BluePrint (..) )
import           Development.Blueprint.Compute.AST       ( parseSourceFile,
                                                           tcModuleToTcGblEnv,
                                                           valBindsToHsBinds )
import           Development.Blueprint.Compute.Morphisms ( entityToGlbRdrElt )
import           Development.Blueprint.Error             ( PipelineError (..) )
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


findModuleName :: (MonadError PipelineError m) => String -> m String
findModuleName = nonMainModule . dropWhile (/= "module") . words
  where nonMainModule []      = throwError EmptySrcFile
        nonMainModule (_:x:_) = return x
        nonMainModule _       = throwError CouldntFindModName


-- | Sets up the right environment for ghc to start doing its job
initializeGhc :: (MonadError PipelineError m, GhcMonad m) => FilePath -> m ModSummary
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
    convertCradle Nothing  = throwError NoCradle

    setupOptions (modName , CradleSuccess r) = do
            dflags <- initSession r >> getSessionDynFlags
            setSessionDynFlags $ dflags { backend = NoBackend, ghcLink = LinkInMemory, ghcMode = CompManager }
            target <- guessTarget filePath Nothing
            setTargets [target] >> load LoadAllTargets
            getModSummary $ mkModuleName modName

    setupOptions _ = throwError FailedCradle


seeFromTcGblEnv :: (GhcMonad m, MonadReader ModSummary m) => (TcGblEnv -> s) -> m s
seeFromTcGblEnv fieldSelector = do
  parsed <- parseSourceFile
  return . fieldSelector . tcModuleToTcGblEnv <=< typecheckModule $ parsed


seeDefUses :: (GhcMonad m, MonadReader ModSummary m) => m DefUses
seeDefUses = seeFromTcGblEnv tcg_dus
