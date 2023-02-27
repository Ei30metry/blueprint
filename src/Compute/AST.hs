module Compute.AST where

import           App                               ( BluePrint (..), bluePrint )

import           Control.Lens.Combinators          ( Field1 (_1), view )
import           Control.Monad                     ( liftM, void, (<=<) )
import           Control.Monad.IO.Class            ( liftIO )
import           Control.Monad.Trans               ( MonadTrans (..) )
import           Control.Monad.Trans.Reader        ( ReaderT, ask, asks, local,
                                                     mapReaderT, reader,
                                                     withReaderT )
import           Control.Monad.Trans.Writer.Lazy   ( WriterT (..), execWriterT,
                                                     mapWriterT )

import           Data.Maybe                        ( fromMaybe )

import           GHC                               ( AnnSortKey, Backend (..),
                                                     DynFlags (backend),
                                                     GhcMonad (getSession),
                                                     GhcRn, GhcT (..),
                                                     HsValBinds (..),
                                                     HsValBindsLR (ValBinds, XValBindsLR),
                                                     HscEnv (..),
                                                     LoadHowMuch (..),
                                                     ModSummary,
                                                     NHsValBindsLR (NValBinds),
                                                     NamedThing (..),
                                                     ParsedModule (ParsedModule),
                                                     Pass (..),
                                                     RenamedSource (..),
                                                     SuccessFlag (..),
                                                     Target (..),
                                                     TypecheckedModule (tm_internals_, tm_renamed_source),
                                                     backend, getModSummary,
                                                     getSession,
                                                     getSessionDynFlags,
                                                     guessTarget, hs_valds,
                                                     load, mkModuleName,
                                                     parseModule,
                                                     setSessionDynFlags,
                                                     setTargets, tm_internals_,
                                                     tm_renamed_source,
                                                     typecheckModule )
import           GHC.Hs.Utils                      ( CollectFlag (..),
                                                     collectHsIdBinders,
                                                     collectHsValBinders,
                                                     spanHsLocaLBinds )
import           GHC.Parser.Annotation             ()
import           GHC.Tc.Module
import           GHC.Tc.Types                      ( TcGblEnv (..), TcRn(..) )
import           GHC.Tc.Utils.Monad
import           GHC.Types.Basic                   ( RecFlag )
import           GHC.Types.Name                    ( Name (..) )
import           GHC.Types.Name.Reader             ( GlobalRdrElt,
                                                     GlobalRdrEnv )
import           GHC.Utils.Panic                   ( panic )

import           Language.Haskell.Syntax.Binds     ( HsValBinds (..) )
import           Language.Haskell.Syntax.Extension ( IdP, NoExtField (..),
                                                     noExtField )

import           Types                             ( Entity (..),
                                                     OutputType (..),
                                                     Scope (..), SearchEnv (..),
                                                     SearchLevel (..) )

parseSourceFile :: forall w m. (Monoid w, GhcMonad m) => BluePrint ModSummary w m ParsedModule
parseSourceFile = BT $ ask >>= \modSum -> lift . lift $ parseModule modSum


tcModuleToTcGblEnv :: TypecheckedModule -> TcGblEnv
tcModuleToTcGblEnv = fst . tm_internals_

typeCheckedToGlbEnv :: TypecheckedModule -> GlobalRdrEnv
typeCheckedToGlbEnv = tcg_rdr_env . tcModuleToTcGblEnv


-- TODO find out in what circumstatnces we have a Nothing value instead of renamed source
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
rnSrcToBindsBP = BT $ ask >>= lift . lift . return . hs_valds . view _1

-- not exported by ghc-lib, so we define it locally
data DataConCantHappen


dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

-- IdP GhcRn ~ Name
valBindsToHsBinds :: HsValBinds GhcRn -> [IdP GhcRn]
valBindsToHsBinds = collectHsValBinders CollNoDictBinders
