module Compute (rnWithGlobalEnv, occNameFromEntity, searchOccName, parseSourceFile, parsedToGlobalRdrEnv', mkFileModName, renamedSourceToBindings) where


import           App                           ( BluePrint )

import           Control.Applicative           ()
import           Control.Monad                 ( join, liftM, mapM, (<=<) )
import           Control.Monad.Extra           ()
import           Control.Monad.IO.Class        ( MonadIO )
import           Control.Monad.Trans           ( MonadTrans, lift, liftIO )
import           Control.Monad.Trans.Maybe     ( MaybeT (..) )
import           Control.Monad.Trans.Reader    ( ReaderT (..), ask, asks, local,
                                                 mapReaderT, withReaderT )
import           Control.Monad.Trans.Writer    ( execWriterT, listens,
                                                 mapWriterT, pass, runWriterT,
                                                 tell )

import           Data.Foldable                 ()
import           Data.Functor                  ( (<&>) )
import           Data.Traversable              ()

import           GHC                           ( Backend (NoBackend), Ghc,
                                                 GhcMonad (getSession), GhcPs,
                                                 GhcRn, HsGroup,
                                                 HsModule (hsmodDecls),
                                                 HsValBinds, LHsDecl,
                                                 LoadHowMuch (LoadAllTargets, LoadUpTo),
                                                 Located,
                                                 ModSummary (ms_hspp_opts),
                                                 Name (..), ParsedMod (..),
                                                 ParsedModule (pm_mod_summary, pm_parsed_source),
                                                 RenamedSource,
                                                 TypecheckedModule (..),
                                                 backend, getModSummary,
                                                 getSessionDynFlags,
                                                 guessTarget, load,
                                                 mkModuleName, parseModule,
                                                 runGhc, setSessionDynFlags,
                                                 setTargets, typecheckModule,
                                                 unLoc )
import           GHC.Data.FastString           ( mkFastString )
import           GHC.Data.IOEnv                ( IOEnv )
import           GHC.Driver.Env
import           GHC.Generics
import           GHC.Paths                     ( libdir )
import           GHC.Rename.Module             ( findSplice, rnSrcDecls )
import           GHC.Tc.Module                 ( RenamedStuff )
import           GHC.Tc.Types                  ( Env, RnM, TcGblEnv (..),
                                                 TcLclEnv (..), TcM, TcRn (..),
                                                 TcRnIf (..) )
import           GHC.Types.Avail
import           GHC.Types.Name                ( OccName (..), mkOccName,
                                                 tcName, varName )
import           GHC.Types.Name.Occurrence     ( NameSpace (..), lookupOccEnv )
import           GHC.Types.Name.Reader         ( GlobalRdrElt (..),
                                                 GlobalRdrEnv (..),
                                                 Parent (NoParent, ParentIs),
                                                 lookupGlobalRdrEnv )
import           GHC.Types.Name.Set            ( NameSet (..) )

import           Language.Haskell.Syntax.Binds ( HsValBinds )
import           Language.Haskell.Syntax.Decls ( HsGroup (..) )

import           Text.Read                     ( readMaybe )

import           Types                         ( Entity (..), EntityOccDef,
                                                 Func, LocalFunc, ParentFunc,
                                                 Scope (..), SearchEnv (..),
                                                 SearchLevel (..), TypeC (..) )

searchImplementation :: a
searchImplementation = undefined


recurseImplementation :: Entity -> SearchLevel -> [a]
recurseImplementation = undefined

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
entityParent (FunctionE s _) = scopeParent s
entityParent _               = NoParent


-- get the occurence name used to define our entity in the source code
getEntityOccString :: Entity -> EntityOccDef
getEntityOccString (DataTypeE t)   = typeName t
getEntityOccString (FunctionE s _) = funcOccString s


funcOccString :: Scope -> Func
funcOccString (TopLevel func)   = func
funcOccString (ParentS _ lFunc) = lFunc

occNameFromEntity :: Entity -> OccName
occNameFromEntity (DataTypeE t)   = mkOccName tcName $ typeName t
occNameFromEntity (FunctionE s _) = mkOccName varName $ funcOccString s

-- computing a parent in order to use it to struct a GlobalRdrElt
scopeParent :: Scope -> Parent
scopeParent (ParentS f _) = ParentIs undefined -- OccName mkNameSpace $ mkFastString f
scopeParent (TopLevel _)  = NoParent

-- converts a

entityToName :: Entity -> Name
entityToName = undefined


-- TODO search for entity in GlobalRdrEnv an if it happend to exist, pick the one that worked
-- and if it didn't, then there is no GlobalRdrElt with the specifications you are looking for

-- lookup an Entity from the

searchOccName :: Monad m => SearchEnv -> GlobalRdrEnv -> m [GlobalRdrElt]
searchOccName sEnv rdrEnv = return $ lookupGlobalRdrEnv rdrEnv (occNameFromEntity . entity $ sEnv)
-- searchOccName = undefined


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


-- Create a viable AST data type
nonMonadicImplmentationFinder :: (GlobalRdrEnv, RenamedSource) -> a
nonMonadicImplmentationFinder = undefined


-- Only need the HomeUnitModule
-- BUG find a way to find the HomeUnitModule
defaultParseMode :: GhcMonad m => FilePath -> m ParsedModule
defaultParseMode = parseSourceFile (LoadUpTo undefined)


parsedToGlobalRdrEnv' :: GhcMonad m => ParsedModule -> m GlobalRdrEnv
parsedToGlobalRdrEnv' parsed = rnWithGlobalEnv parsed <&> fst


type RenamedBinds = HsValBinds GhcRn

renamedSourceToBindings :: Monad m => RenamedSource -> m (HsValBinds GhcRn)
renamedSourceToBindings = return . hs_valds . \(b,_,_,_) -> b

parseToGlobalRdrEnv :: ReaderT ParsedModule Maybe GlobalRdrEnv
parseToGlobalRdrEnv = undefined
