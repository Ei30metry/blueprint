module Compute.AST where

import           App                               ( BluePrint (..) )

import           Control.Lens.Combinators          ( Field1 (_1), view )
import           Control.Monad                     ( liftM, void, (<=<) )
import           Control.Monad.IO.Class            ( liftIO )
import           Control.Monad.Trans               ( MonadTrans (..) )
import           Control.Monad.Trans.Reader        ( ReaderT, ask, asks, local,
                                                     mapReaderT, reader,
                                                     withReaderT )
import           Control.Monad.Trans.Writer.Lazy   ( WriterT (..), execWriterT,
                                                     mapWriterT )

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
import           GHC.Tc.Types                      ( TcGblEnv (tcg_rdr_env) )
import           GHC.Types.Basic                   ( RecFlag )
import           GHC.Types.Name                    ( Name (..) )
import           GHC.Types.Name.Reader             ( GlobalRdrElt,
                                                     GlobalRdrEnv )

import           Language.Haskell.Syntax.Binds     ( HsValBinds (..) )
import           Language.Haskell.Syntax.Extension ( IdP, NoExtField (..),
                                                     noExtField )

import           Types                             ( Entity (..),
                                                     OutputType (..),
                                                     Scope (..), SearchEnv (..),
                                                     SearchLevel (..) )


rnWithGlobalEnv' :: GhcMonad m => ParsedModule -> m (GlobalRdrEnv, Maybe RenamedSource)
rnWithGlobalEnv' = return . glbWithRenamed <=< typecheckModule
  where glbWithRenamed tChecked = (tcg_rdr_env . fst . tm_internals_ $ tChecked, tm_renamed_source tChecked)


rnWithGlobalEnv :: forall w m. (GhcMonad m, Monoid w) => BluePrint ParsedModule w m (GlobalRdrEnv, Maybe RenamedSource)
rnWithGlobalEnv = BT $ do
    parsedAST <- ask
    lift . lift $ go parsedAST
  where
    go = return . glbWithRenamed <=< typecheckModule
    glbWithRenamed tcd = (tcg_rdr_env . fst . tm_internals_ $ tcd, tm_renamed_source tcd)


parseSourceFile :: forall w m. (Monoid w, GhcMonad m) => BluePrint ModSummary w m ParsedModule
parseSourceFile = BT $ ask >>= \modSum -> lift . lift $ parseModule modSum


rnSrcToBinds :: forall m w. (GhcMonad m, Monoid w) => BluePrint RenamedSource w m (HsValBinds GhcRn)
rnSrcToBinds = BT $ ask >>= lift . lift . return . hs_valds . view _1

-- not exported by ghc-lib, so we define it locally
data DataConCantHappen

dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

-- IdP GhcRn ~ Name
valBindsToHsBinds :: HsValBinds GhcRn -> [IdP GhcRn]
valBindsToHsBinds = collectHsValBinders CollNoDictBinders
