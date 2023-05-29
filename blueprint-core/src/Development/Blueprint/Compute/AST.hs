module Development.Blueprint.Compute.AST where

import           Control.Lens.Combinators        ( Field1 (_1), view )
import           Control.Monad                   ( (<=<) )
import           Control.Monad.Except            ( MonadError (..) )
import           Control.Monad.Reader            ( MonadReader (..) )

import           Data.Functor                    ( (<&>) )

import           Development.Blueprint.Error     ( PipelineError (GhcCouldntRename) )
import           Development.Blueprint.Types.AST ( DataConCantHappen )

import           GHC                             ( GhcMonad (getSession),
                                                   ModSummary,
                                                   ParsedModule (..),
                                                   RenamedSource (..),
                                                   TypecheckedModule (tm_internals_, tm_renamed_source),
                                                   backend, hs_valds,
                                                   parseModule, tm_internals_,
                                                   tm_renamed_source,
                                                   typecheckModule )
import           GHC.Hs                          ( CollectFlag (CollNoDictBinders),
                                                   GhcRn, HsValBinds, IdP,
                                                   collectHsValBinders )
import           GHC.Tc.Types                    ( TcGblEnv (..) )
import           GHC.Types.Name.Reader           ( GlobalRdrEnv )



parseSourceFile :: (GhcMonad m, MonadReader ModSummary m) => m ParsedModule
parseSourceFile = ask >>= parseModule


tcModuleToTcGblEnv :: TypecheckedModule -> TcGblEnv
tcModuleToTcGblEnv = fst . tm_internals_

typeCheckedToGlbEnv :: TypecheckedModule -> GlobalRdrEnv
typeCheckedToGlbEnv = tcg_rdr_env . tcModuleToTcGblEnv


typeCheckedToRenamed :: (MonadError PipelineError m) => TypecheckedModule -> m RenamedSource
typeCheckedToRenamed tchecked = case tm_renamed_source tchecked of
  Just x  -> return x
  Nothing -> throwError GhcCouldntRename


rnWithGlobalEnv :: (GhcMonad m, MonadError PipelineError m, MonadReader ParsedModule m) => m (GlobalRdrEnv, RenamedSource)
rnWithGlobalEnv = do
    parsedAST <- ask
    go parsedAST
  where
    go = glbWithRenamed <=< typecheckModule
    glbWithRenamed tcd = case typeCheckedToRenamed tcd of
      Right x  -> return (typeCheckedToGlbEnv tcd, x)
      Left err -> throwError err


rnSrcToBindsBP :: (GhcMonad m, MonadReader RenamedSource m) => m (HsValBinds GhcRn)
rnSrcToBindsBP = ask <&> hs_valds . view _1

dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

valBindsToHsBinds :: HsValBinds GhcRn -> [IdP GhcRn]
valBindsToHsBinds = collectHsValBinders CollNoDictBinders
