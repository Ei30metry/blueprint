module Development.Blueprint.Compute.AST where

import           Control.Lens.Combinators        ( Field1 (_1), view )
import           Control.Monad                   ( (<=<) )
import           Control.Monad.Trans             ( MonadTrans (..) )
import           Control.Monad.Trans.Reader      ( ask )

import           Data.Functor                    ( (<&>) )
import           Data.Maybe                      ( fromMaybe )
import           Data.Tree                       ( Tree (..) )

import           Development.Blueprint.App       ( BluePrint (..) )
import           Development.Blueprint.Error
import           Development.Blueprint.Types.AST ( DataConCantHappen )

import           GHC                             ( GhcMonad (getSession), GhcRn,
                                                   HsValBinds (..), ModSummary,
                                                   ParsedModule (..),
                                                   RenamedSource (..),
                                                   TypecheckedModule (tm_internals_, tm_renamed_source),
                                                   backend, hs_valds,
                                                   parseModule, tm_internals_,
                                                   tm_renamed_source,
                                                   typecheckModule )
import           GHC.Generics                    ( Generic )
import           GHC.Hs
import           GHC.Hs.Utils                    ( CollectFlag (..),
                                                   collectHsValBinders )
import           GHC.Tc.Types                    ( TcGblEnv (..) )
import           GHC.Types.Name.Reader           ( GlobalRdrEnv )
import           GHC.Utils.Panic                 ( panic )
import Control.Monad.Trans.Except (throwE)



parseSourceFile :: forall w m e. (Monoid w, GhcMonad m) => BluePrint e ModSummary w m ParsedModule
parseSourceFile = BT $ ask >>= \modSum -> lift . lift . lift $ parseModule modSum


tcModuleToTcGblEnv :: TypecheckedModule -> TcGblEnv
tcModuleToTcGblEnv = fst . tm_internals_

typeCheckedToGlbEnv :: TypecheckedModule -> GlobalRdrEnv
typeCheckedToGlbEnv = tcg_rdr_env . tcModuleToTcGblEnv


-- lift to ExceptT
typeCheckedToRenamed :: TypecheckedModule -> Either PipelineError RenamedSource
typeCheckedToRenamed tchecked = case tm_renamed_source tchecked of
  Just x -> Right x
  Nothing -> Left GhcCouldntRename


rnWithGlobalEnv :: forall w m e. (GhcMonad m, Monoid w) => BluePrint PipelineError ParsedModule w m (GlobalRdrEnv, RenamedSource)
rnWithGlobalEnv = BT $ do
    parsedAST <- ask
    lift $ go parsedAST
  where
    go = glbWithRenamed <=< (lift . lift . typecheckModule)
    glbWithRenamed tcd = case typeCheckedToRenamed tcd of
      Right x -> return (typeCheckedToGlbEnv tcd, x)
      Left err -> throwE err


rnSrcToBindsBP :: forall m w e. (GhcMonad m, Monoid w) => BluePrint e RenamedSource w m (HsValBinds GhcRn)
rnSrcToBindsBP = BT $ ask <&> hs_valds . view _1

-- not exported by ghc-lib, so we define it locally
dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

-- IdP GhcRn ~ Name
valBindsToHsBinds :: HsValBinds GhcRn -> [IdP GhcRn]
valBindsToHsBinds = collectHsValBinders CollNoDictBinders
