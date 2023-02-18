module Compute.AST where

import           App                               ( BluePrint (..),
                                                     mkFileModName )

import           Control.Monad                     ( liftM, void, (<=<) )
import           Control.Monad.IO.Class            ( liftIO )
import           Control.Monad.Trans               ( MonadTrans (..) )
import           Control.Monad.Trans.Reader        ( ReaderT, ask, asks, local,
                                                     mapReaderT, reader,
                                                     withReaderT )
import           Control.Monad.Trans.Writer.Lazy   ( WriterT (..), execWriterT,
                                                     mapWriterT )

import           Data.Coerce                       ( coerce )

import           GHC                               ( AnnSortKey, Backend (..),
                                                     DynFlags (backend),
                                                     GhcMonad (getSession),
                                                     GhcRn, GhcT (..),
                                                     HsValBinds (..),
                                                     HsValBindsLR (ValBinds, XValBindsLR),
                                                     HscEnv (..),
                                                     LoadHowMuch (..),
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
import           GHC.Plugins                       ( panic )
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


data GhcEnv = GhcEnv { ourHscEnv     :: HscEnv
                     , ourDynFlags   :: DynFlags
                     , currentTarget :: Target
                     , sucessFlag    :: SuccessFlag }


rnWithGlobalEnv :: GhcMonad m => ParsedModule -> m (GlobalRdrEnv, Maybe RenamedSource)
rnWithGlobalEnv = return . glbWithRenamed <=< typecheckModule
  where glbWithRenamed tChecked = (tcg_rdr_env . fst . tm_internals_ $ tChecked, tm_renamed_source tChecked)

-- TODO implement both with ReaderT and without ReaderT
initializeEnv :: GhcMonad m => FilePath -> m GhcEnv
initializeEnv fp = do
  let fileModuleName = mkFileModName fp
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags {backend = NoBackend}
  target <- guessTarget fp Nothing
  setTargets [target]
  succFlag <- load LoadAllTargets
  return $ GhcEnv env dflags target succFlag

parseSourceFile :: GhcMonad m => LoadHowMuch -> FilePath -> m ParsedModule
parseSourceFile loadHowMuch filePath = do
  let fileModuleName = mkFileModName filePath
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { backend = NoBackend }
  target <- guessTarget filePath Nothing
  setTargets [target]
  load loadHowMuch -- TODO construct a Unit in order to feed your path to your module
  modSum <- getModSummary $ mkModuleName fileModuleName
  parseModule modSum


parseSourceFile' :: GhcMonad m => FilePath -> m ParsedModule
parseSourceFile' filePath = getModSummary (mkModuleName $ mkFileModName filePath) >>= parseModule


rnSrcToBinds :: GhcMonad m => RenamedSource -> m (HsValBinds GhcRn)
rnSrcToBinds = return . hs_valds . \(b,_,_,_) -> b


-- defaultParseMode :: GhcMonad m => FilePath -> m ParsedModule
-- defaultParseMode = parseSourceFile LoadAllTargets


-- not exported by ghc-lib, so we define it locally
data DataConCantHappen

dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

-- IdP GhcRn ~ Name
valBindsToHsBinds :: HsValBinds GhcRn -> [IdP GhcRn]
valBindsToHsBinds = collectHsValBinders CollNoDictBinders

-- prototypeFunc :: GhcMonad m => FilePath -> m [(RecFlag, LHsBinds GhcRn)]
-- BUG find out the source of renamed source being in a maybe context and then try to avoid panic
prototypeFunc :: GhcMonad m => FilePath -> m [IdP GhcRn]
prototypeFunc = return . valBindsToHsBinds <=< rnSrcToBinds <=< handleRenamed . snd <=< rnWithGlobalEnv <=< parseSourceFile LoadAllTargets
  where handleRenamed (Just x) = return x
        handleRenamed Nothing  = panic "Couldln't run our parsed module through Renamer"


prototypeFuncB' :: GhcMonad m => ReaderT FilePath (WriterT String m) [Name]
prototypeFuncB' = ask >>= \filePath -> lift . lift $ prototypeFunc filePath
