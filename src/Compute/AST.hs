module Compute.AST where

import           Control.Monad         ( (<=<) )

import           GHC                   ( Backend (..), DynFlags (backend),
                                         GhcMonad (getSession), GhcRn,
                                         HsValBinds, LoadHowMuch (..),
                                         ParsedModule (ParsedModule),
                                         RenamedSource (..),
                                         TypecheckedModule (tm_internals_, tm_renamed_source),
                                         backend, getModSummary, getSession,
                                         getSessionDynFlags, guessTarget,
                                         hs_valds, load, mkModuleName,
                                         parseModule, setSessionDynFlags,
                                         setTargets, tm_internals_,
                                         tm_renamed_source, typecheckModule )
import           GHC.Parser.Annotation ()
import           GHC.Tc.Types          ( TcGblEnv (tcg_rdr_env) )
import           GHC.Types.Name.Reader ( GlobalRdrElt, GlobalRdrEnv )

import           Types                 ( Entity (..), OutputType (..),
                                         Scope (..), SearchEnv (..),
                                         SearchLevel (..) )


renamedSourceToBindings :: Monad m => RenamedSource -> m (HsValBinds GhcRn)
renamedSourceToBindings = return . hs_valds . \(b,_,_,_) -> b


defaultParseMode :: GhcMonad m => FilePath -> m ParsedModule
defaultParseMode = parseSourceFile (LoadUpTo undefined)


mkFileModName :: FilePath -> String
mkFileModName = reverse . takeWhile (/= '/') . reverse . (\fp -> take (length fp - 3) fp)

rnWithGlobalEnv :: GhcMonad m => ParsedModule -> m (GlobalRdrEnv, Maybe RenamedSource)
rnWithGlobalEnv = return . glbWithRenamed <=< typecheckModule
  where glbWithRenamed tChecked = (tcg_rdr_env . fst . tm_internals_$ tChecked, tm_renamed_source tChecked)

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
