module Compute.AST where

import           App                           ( mkFileModName, setUpGhc )

import           Control.Monad                 ( (<=<) )

import           GHC                           ( AnnSortKey, Backend (..),
                                                 DynFlags (backend),
                                                 GhcMonad (getSession), GhcRn,
                                                 GhcT (..), HsValBinds (..),
                                                 HsValBindsLR (ValBinds, XValBindsLR),
                                                 LoadHowMuch (..),
                                                 NHsValBindsLR (NValBinds),
                                                 ParsedModule (ParsedModule),
                                                 Pass (..), RenamedSource (..),
                                                 SuccessFlag (..),
                                                 TypecheckedModule (tm_internals_, tm_renamed_source),
                                                 backend, getModSummary,
                                                 getSession, getSessionDynFlags,
                                                 guessTarget, hs_valds, load,
                                                 mkModuleName, parseModule,
                                                 setSessionDynFlags, setTargets,
                                                 tm_internals_,
                                                 tm_renamed_source,
                                                 typecheckModule )
import           GHC.Parser.Annotation         ()
import           GHC.Tc.Types                  ( TcGblEnv (tcg_rdr_env) )
import           GHC.Types.Basic               ( RecFlag )
import           GHC.Types.Name                ( Name (..) )
import           GHC.Types.Name.Reader         ( GlobalRdrElt, GlobalRdrEnv )

import           Language.Haskell.Syntax.Binds

import           Types                         ( Entity (..), OutputType (..),
                                                 Scope (..), SearchEnv (..),
                                                 SearchLevel (..) )


rnWithGlobalEnv :: GhcMonad m => ParsedModule -> m (GlobalRdrEnv, Maybe RenamedSource)
rnWithGlobalEnv = return . glbWithRenamed <=< typecheckModule
  where glbWithRenamed tChecked = (tcg_rdr_env . fst . tm_internals_ $ tChecked, tm_renamed_source tChecked)


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


renamedSourceToBindings :: GhcMonad m => RenamedSource -> m (HsValBinds GhcRn)
renamedSourceToBindings = return . hs_valds . \(b,_,_,_) -> b


defaultParseMode :: GhcMonad m => FilePath -> m ParsedModule
defaultParseMode = parseSourceFile (LoadUpTo undefined)


-- prototypeFunc :: GhcMonad m => FilePath -> m ()
-- prototypeFunc :: GhcMonad m => FilePath -> m (NHsValBindsLR GhcRn)
prototypeFunc :: GhcMonad m => FilePath -> m [(RecFlag, LHsBinds GhcRn)]
prototypeFunc = handleBinds <=< renamedSourceToBindings <=< handleRenamed . snd <=< rnWithGlobalEnv <=< parseSourceFile LoadAllTargets
  where handleRenamed (Just x) = return x
        handleRenamed Nothing  = return undefined -- BUG findout when we can't rename a parsed module and handle this
                                          -- explicitly
        handleBinds (XValBindsLR (NValBinds a b)) = return a
