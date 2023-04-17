module Compute.AST where

import           App                               ( BluePrint (..) )

import           Control.Lens.Combinators          ( Field1 (_1), view )
import           Control.Monad                     ( (<=<) )
import           Control.Monad.Trans               ( MonadTrans (..) )
import           Control.Monad.Trans.Reader        ( ask )

import           Data.Aeson                        ( ToJSON (..), object, (.=) )
import           Data.Functor                      ( (<&>) )
import           Data.Maybe                        ( fromMaybe )
import           Data.Tree                         ( Tree (..) )

import           GHC                               ( GhcMonad (getSession),
                                                     GhcRn, HsValBinds (..),
                                                     ModSummary,
                                                     ParsedModule (..),
                                                     RenamedSource (..),
                                                     TypecheckedModule (tm_internals_, tm_renamed_source),
                                                     backend, hs_valds,
                                                     parseModule, tm_internals_,
                                                     tm_renamed_source,
                                                     typecheckModule )
import           GHC.Generics                      ( Generic )
import           GHC.Hs.Utils                      ( CollectFlag (..),
                                                     collectHsValBinders )
import           GHC.Tc.Types                      ( TcGblEnv (..) )
import           GHC.Types.Name.Reader             ( GlobalRdrEnv )
import           GHC.Utils.Panic                   ( panic )

import           Language.Haskell.Syntax.Extension ( IdP )


import           Types.AST                         ( DataConCantHappen )



parseSourceFile :: forall w m. (Monoid w, GhcMonad m) => BluePrint ModSummary w m ParsedModule
parseSourceFile = BT $ ask >>= \modSum -> lift . lift $ parseModule modSum


tcModuleToTcGblEnv :: TypecheckedModule -> TcGblEnv
tcModuleToTcGblEnv = fst . tm_internals_

typeCheckedToGlbEnv :: TypecheckedModule -> GlobalRdrEnv
typeCheckedToGlbEnv = tcg_rdr_env . tcModuleToTcGblEnv


typeCheckedToRenamed :: TypecheckedModule -> RenamedSource
typeCheckedToRenamed = fromMaybe fix . tm_renamed_source
  where fix = panic explanation
        explanation = "This shouldn't have happend. GHC couldn't rename the parsed module."


rnWithGlobalEnv :: forall w m. (GhcMonad m, Monoid w) => BluePrint ParsedModule w m (GlobalRdrEnv, RenamedSource)
rnWithGlobalEnv = BT $ do
    parsedAST <- ask
    lift . lift $ go parsedAST
  where
    go = return . glbWithRenamed <=< typecheckModule
    glbWithRenamed tcd = (typeCheckedToGlbEnv tcd, typeCheckedToRenamed tcd)


rnWithGlobalEnv' :: GhcMonad m => ParsedModule -> m (GlobalRdrEnv, RenamedSource)
rnWithGlobalEnv' = return . glbWithRenamed <=< typecheckModule
  where glbWithRenamed tcd = (typeCheckedToGlbEnv tcd, typeCheckedToRenamed tcd)


rnSrcToBindsBP :: forall m w. (GhcMonad m, Monoid w) => BluePrint RenamedSource w m (HsValBinds GhcRn)
rnSrcToBindsBP = BT $ ask <&> hs_valds . view _1

-- not exported by ghc-lib, so we define it locally
dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

-- IdP GhcRn ~ Name
valBindsToHsBinds :: HsValBinds GhcRn -> [IdP GhcRn]
valBindsToHsBinds = collectHsValBinders CollNoDictBinders
