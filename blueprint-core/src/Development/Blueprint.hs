{-
This module is the frontend to all the functionalities provided by blueprint and
should be sufficient for most use cases
-}

module Development.Blueprint where

import           Control.Monad.Except              ( MonadError (..) )
import           Control.Monad.Reader              ( MonadIO (liftIO),
                                                     MonadReader, ask, (<=<) )

import           Development.Blueprint.Compute.AST ( tcModuleToTcGblEnv )
import           Development.Blueprint.Error       ( PipelineError (..) )

import           GHC                               ( Backend (..), ClsInst,
                                                     DynFlags (backend, ghcLink, ghcMode),
                                                     FamInst,
                                                     GhcLink (LinkInMemory),
                                                     GhcMode (CompManager),
                                                     GhcPs, GhcRn, GhcTc,
                                                     HsExpr (HsUnboundVar, HsVar),
                                                     HsGroup (hs_annds, hs_valds),
                                                     LHsBinds, LHsExpr,
                                                     LImportDecl,
                                                     LoadHowMuch (..),
                                                     ModSummary (..), Module,
                                                     Name, ParsedModule (..),
                                                     RealSrcLoc, RealSrcSpan,
                                                     TyCon, backend,
                                                     getModSummary,
                                                     getSessionDynFlags,
                                                     guessTarget, hs_valds,
                                                     load, mkModuleName,
                                                     setSessionDynFlags,
                                                     setTargets,
                                                     typecheckModule, HsRecField' (HsRecField) )
import           GHC.Core.FamInstEnv               ( FamInstEnv )
import           GHC.Core.InstEnv                  ( InstEnv )
import           GHC.Core.PatSyn                   ( PatSyn )
import           GHC.Driver.Monad                  ( GhcMonad (..) )
import           GHC.Plugins                       ( AnnEnv, Annotation )
import           GHC.Tc.Types                      ( TcGblEnv (..) )
import           GHC.Types.Name.Reader             ( GlobalRdrEnv )
import           GHC.Types.Name.Set                ( DefUses )
import           GHC.Types.TypeEnv                 ( TypeEnv )

import           HIE.Bios                          ( CradleLoadResult (CradleSuccess),
                                                     findCradle,
                                                     getCompilerOptions,
                                                     loadCradle )
import           HIE.Bios.Environment              ( initSession )

import           Language.Haskell.Syntax.Expr
import           Language.Haskell.Syntax.Decls
import           Language.Haskell.Syntax.Binds

import           System.Directory                  ( getCurrentDirectory )


-- | Tries to find the module name by looking for the 'module' keyword
findModuleName :: (MonadError PipelineError m) => String -> m String
findModuleName = nonMainModule . dropWhile (/= "module") . words
  where nonMainModule []      = throwError EmptySrcFile
        nonMainModule (_:x:_) = return x
        nonMainModule _       = throwError CouldntFindModName


-- | A variant of findModuleName which will assume a source without declare module name is the Main module
findModuleName' :: (MonadError PipelineError m) => String -> m String
findModuleName' = nonMainModule . dropWhile (/= "module") . words
  where nonMainModule []      = throwError EmptySrcFile
        nonMainModule (_:x:_) = return x
        nonMainModule _       = return "Main"


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


-- This constraint synonym is defined only for better readability in type signatures
class (GhcMonad m, MonadReader a m) => GhcReader a m | m -> a
instance (GhcMonad m, MonadReader a m) => GhcReader a m


-- | Gets a field selector from TcGblEnv and then runs it on the ModSummary
seeFromTcGblEnv :: GhcReader ParsedModule m => (TcGblEnv -> s) -> m s
seeFromTcGblEnv fieldSelector = (return . fieldSelector . tcModuleToTcGblEnv <=< typecheckModule) =<< ask

-- | Utility functions for accessing fields from TcGblEnv

seeDefUses :: GhcReader ParsedModule m => m DefUses
seeDefUses = seeFromTcGblEnv tcg_dus


seeModule :: GhcReader ParsedModule m => m Module
seeModule = seeFromTcGblEnv tcg_mod


seeGlobalRdrEnv :: GhcReader ParsedModule m => m GlobalRdrEnv
seeGlobalRdrEnv = seeFromTcGblEnv tcg_rdr_env


seeFamilyInstEnv :: GhcReader ParsedModule m => m FamInstEnv
seeFamilyInstEnv = seeFromTcGblEnv tcg_fam_inst_env


seeTypeEnv :: GhcReader ParsedModule m => m TypeEnv
seeTypeEnv = seeFromTcGblEnv tcg_type_env


seeInstEnv :: GhcReader ParsedModule m => m InstEnv
seeInstEnv = seeFromTcGblEnv tcg_inst_env


seeAnnEnv :: GhcReader ParsedModule m => m AnnEnv
seeAnnEnv = seeFromTcGblEnv tcg_ann_env


seeRenamedDecls :: (GhcReader ParsedModule m, MonadError PipelineError m) => m (HsGroup GhcRn)
seeRenamedDecls = seeFromTcGblEnv tcg_rn_decls >>= reportRenamed
  where reportRenamed Nothing  = throwError GhcCouldntRename
        reportRenamed (Just x) = return x


seeBinds :: GhcReader ParsedModule m => m (LHsBinds GhcTc)
seeBinds = seeFromTcGblEnv tcg_binds


seeTyCons :: GhcReader ParsedModule m => m [TyCon]
seeTyCons = seeFromTcGblEnv tcg_tcs


seeInsts :: GhcReader ParsedModule m => m [ClsInst]
seeInsts = seeFromTcGblEnv tcg_insts


seeFamilyInsts :: GhcReader ParsedModule m => m [FamInst]
seeFamilyInsts = seeFromTcGblEnv tcg_fam_insts


seeRealSrcSpan :: GhcReader ParsedModule m => m RealSrcSpan
seeRealSrcSpan = seeFromTcGblEnv tcg_top_loc


seePatternSynonyms :: GhcReader ParsedModule m => m [PatSyn]
seePatternSynonyms = seeFromTcGblEnv tcg_patsyns


seeAnnotations :: GhcReader ParsedModule m => m [Annotation]
seeAnnotations = seeFromTcGblEnv tcg_anns


seeImports :: GhcReader ParsedModule m => m [LImportDecl GhcRn]
seeImports = seeFromTcGblEnv tcg_rn_imports


substitute :: HsExpr GhcPs -> HsExpr GhcPs
substitute (HsVar a b)        = undefined
substitute (HsUnboundVar a b) = undefined
substitute (HsRecFld a b )     = undefined
substitute (HsOverLabel a b)        = undefined
substitute (HsIPVar a b)        = undefined
substitute (HsOverLit a b )        = undefined
substitute (HsLit a b )        = undefined
substitute (HsLam a b )        = undefined
substitute (HsLamCase a b) = undefined
substitute (HsApp a b c) = undefined
substitute (HsAppType a b c) = undefined
substitute (OpApp a b c d) = undefined
substitute (OpApp a b c d) = undefined
substitute (NegApp a b c) = undefined
substitute (HsPar a b) = undefined
substitute (SectionL a b c) = undefined
substitute (SectionR a b c) = undefined
substitute (ExplicitTuple a b c) = undefined
substitute (ExplicitSum a b c d) = undefined
substitute (HsCase a b c) = undefined
substitute (HsIf a b c d) = undefined
substitute (HsMultiIf a b) = undefined
substitute (HsLet a b c) = undefined
substitute (HsDo a b c) = undefined
substitute (ExplicitList a b) = undefined
substitute (RecordCon a b c) = undefined
substitute (RecordUpd a b c) = undefined
substitute (HsGetField a b c) = undefined
substitute (HsProjection a b) = undefined
substitute _ = undefined
