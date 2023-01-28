module Compute (rnWithGlobalEnv, occNameFromEntity, searchOccName, parseSourceFile, parsedToGlobalRdrEnv') where


import           App                        ( BluePrint )

import           Control.Applicative        ()
import           Control.Monad              ( join, liftM, mapM, (<=<) )
import           Control.Monad.Extra        ()
import           Control.Monad.IO.Class     ( MonadIO )
import           Control.Monad.Trans        ( MonadTrans, lift, liftIO )
import           Control.Monad.Trans.Maybe  ( MaybeT (..) )
import           Control.Monad.Trans.Reader ( ReaderT (..), ask, asks, local,
                                              mapReaderT, withReaderT )

import           Data.Foldable              ()
import           Data.Functor               ( (<&>) )
import           Data.Traversable           ()

import           GHC                        ( Backend (NoBackend), Ghc,
                                              GhcMonad (getSession), GhcPs,
                                              GhcRn, HsGroup,
                                              HsModule (hsmodDecls), LHsDecl,
                                              LoadHowMuch (LoadAllTargets),
                                              Located,
                                              ModSummary (ms_hspp_opts),
                                              Name (..), ParsedMod (..),
                                              ParsedModule (pm_mod_summary, pm_parsed_source),
                                              RenamedSource,
                                              TypecheckedModule (..), backend,
                                              getModSummary, getSessionDynFlags,
                                              guessTarget, load, mkModuleName,
                                              parseModule, runGhc,
                                              setSessionDynFlags, setTargets,
                                              typecheckModule, unLoc )
import           GHC.Data.FastString        ( mkFastString )
import           GHC.Data.IOEnv             ( IOEnv )
import           GHC.Driver.Env
import           GHC.Generics
import           GHC.Paths                  ( libdir )
import           GHC.Rename.Module          ( findSplice, rnSrcDecls )
import           GHC.Tc.Module              ( RenamedStuff )
import           GHC.Tc.Types               ( Env, RnM, TcGblEnv (..),
                                              TcLclEnv (..), TcM, TcRn (..),
                                              TcRnIf (..) )
import           GHC.Types.Name             ( OccName (..), mkOccName, tcName,
                                              varName )
import           GHC.Types.Name.Occurrence  ( NameSpace (..), lookupOccEnv )
import           GHC.Types.Name.Reader      ( GlobalRdrElt (..),
                                              GlobalRdrEnv (..),
                                              Parent (NoParent, ParentIs),
                                              lookupGlobalRdrEnv )
import           GHC.Types.Name.Set

import           Text.Read                  ( readMaybe )

import           Types                      ( Entity (..), EntityOccDef, Func,
                                              LocalFunc, ParentFunc, Scope (..),
                                              SearchEnv (..), SearchLevel (..),
                                              TypeC (..) )

searchImplementation :: a
searchImplementation = undefined


-- This function only works if the Module name is the same as file name
mkFileModName :: FilePath -> String
mkFileModName = reverse . takeWhile (/= '/') . reverse . (\fp -> take (length fp - 3) fp)


rnWithGlobalEnv :: GhcMonad m => ParsedModule -> m (GlobalRdrEnv, Maybe RenamedSource)
rnWithGlobalEnv = return . glbWithRenamed <=< typecheckModule
  where glbWithRenamed tChecked = (tcg_rdr_env . fst . tm_internals_$ tChecked, tm_renamed_source tChecked)


-- NameSpace == Varname && NameSort == External ....
-- lookupName :: GlobalRdrEnv -> String -> [Name]

sourceToGlobalRdrEnv :: FilePath -> ReaderT FilePath Maybe GlobalRdrEnv
sourceToGlobalRdrEnv filePath = undefined

entityParent :: Entity -> Parent
entityParent (FunctionE s) = scopeParent s
entityParent _             = NoParent


-- get the occurence name used to define our entity in the source code
getEntityOccString :: Entity -> EntityOccDef
getEntityOccString (DataTypeE t) = typeName t
getEntityOccString (FunctionE s) = funcOccString s


funcOccString :: Scope -> Func
funcOccString (TopLevel func)   = func
funcOccString (ParentS _ lFunc) = lFunc

occNameFromEntity :: Entity -> OccName
occNameFromEntity (DataTypeE t) = mkOccName tcName $ typeName t
occNameFromEntity (FunctionE s) = mkOccName varName $ funcOccString s

-- computing a parent in order to use it to struct a GlobalRdrElt
scopeParent :: Scope -> Parent
scopeParent (ParentS f _) = ParentIs undefined -- OccName mkNameSpace $ mkFastString f
scopeParent (TopLevel _)  = NoParent

-- converts a

entityToName :: Entity -> Name
entityToName = undefined


-- TODO search for entity in GlobalRdrEnv an if it happend to exist, pick the one that worked
-- and if it didn't, then there is no GlobalRdrElt with the specifications you are looking for
-- entityToGlobalRdrElt :: Entity -> GlobalRdrElt
-- entityToGlobalRdrElt ent = GRE targetName targetParent True targetImp
--   where targetName = undefined
--         targetParent = undefined
--         targetImp = undefined


-- lookup an Entity from the
lookupEntity :: BluePrint GlobalRdrEnv [GlobalRdrElt]
lookupEntity  = undefined


toHsGroup :: ParsedModule -> IOEnv (Env TcGblEnv TcLclEnv) (TcGblEnv, HsGroup GhcRn)
toHsGroup = rnSrcDecls <=< fstM <=< findSplice . hsmodDecls . unLoc . pm_parsed_source
  where fstM = return . fst


searchOccName :: Monad m => SearchEnv -> GlobalRdrEnv -> m [GlobalRdrElt]
searchOccName sEnv rdrEnv = return $ lookupGlobalRdrEnv rdrEnv (occNameFromEntity . entity $ sEnv)


parseSourceFile :: GhcMonad m => FilePath -> m ParsedModule
parseSourceFile filePath = do
  let fileModuleName = mkFileModName filePath
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { backend = NoBackend }
  target <- guessTarget filePath Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName fileModuleName
  parseModule modSum


parsedToGlobalRdrEnv' :: GhcMonad m => ParsedModule -> m GlobalRdrEnv
parsedToGlobalRdrEnv' parsed = rnWithGlobalEnv parsed <&> fst


parseToGlobalRdrEnv :: ReaderT ParsedModule Maybe GlobalRdrEnv
parseToGlobalRdrEnv = undefined
