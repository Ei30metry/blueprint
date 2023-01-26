module Compute (rnWithGlobalEnv) where


import           App                        ( BluePrint )

import           Control.Applicative        ()
import           Control.Monad              ( join, liftM, mapM, (<=<), (>=>) )
import           Control.Monad.Extra        ()
import           Control.Monad.Trans        ( MonadTrans, lift, liftIO )
import           Control.Monad.Trans.Class  ()
import           Control.Monad.Trans.Reader ( ReaderT (..), ask, asks, local,
                                              mapReaderT, withReaderT )

import           Data.Foldable              ()
import           Data.IntMap.Lazy           ( lookup )
import qualified Data.Map                   as M
import           Data.Text                  ( pack )
import           Data.Traversable           ()

import           GHC                        ( GhcMonad (getSession), GhcPs,
                                              GhcRn, HsGroup,
                                              HsModule (hsmodDecls), LHsDecl,
                                              LoadHowMuch (LoadAllTargets),
                                              Located,
                                              ModSummary (ms_hspp_opts),
                                              Name (..), ParsedMod (..),
                                              ParsedModule (pm_mod_summary, pm_parsed_source),
                                              RenamedSource,
                                              TypecheckedModule (..),
                                              getModSummary, getSessionDynFlags,
                                              guessTarget, load, mkModuleName,
                                              parseModule, setSessionDynFlags,
                                              typecheckModule, unLoc )
import           GHC.Data.FastString        ( mkFastString )
import           GHC.Driver.Env
import           GHC.Generics
import           GHC.Rename.Module          ( findSplice, rnSrcDecls )
import           GHC.Tc.Module              ( RenamedStuff )
import           GHC.Tc.Types               ( RnM, TcGblEnv (..), TcLclEnv (..),
                                              TcM, TcRn (..), TcRnIf (..) )
import           GHC.Types.Name             ( OccName, mkOccName )
import           GHC.Types.Name.Occurrence  ( lookupOccEnv )
import           GHC.Types.Name.Reader      ( GlobalRdrElt (..),
                                              GlobalRdrEnv (..) )

import           Text.Read                  ( readMaybe )

import           Types                      ( Entity (..), Scope (..),
                                              SearchEnv (..), SearchLevel (..),
                                              TypeC (..) )

searchImplementation :: a
searchImplementation = undefined


rnWithGlobalEnv :: GhcMonad m => ParsedModule -> m (GlobalRdrEnv, Maybe RenamedSource)
rnWithGlobalEnv = return . glbWithRenamed <=< typecheckModule
  where glbWithRenamed tChecked = (tcg_rdr_env . fst . tm_internals_$ tChecked, tm_renamed_source tChecked)



-- NameSpace == Varname && NameSort == External ....
-- lookupName :: GlobalRdrEnv -> String -> [Name]
lookupName :: BluePrint GlobalRdrEnv Name
lookupName = undefined

lookupManyNames :: [String] -> [Maybe Name]
lookupManyNames = undefined

-- toHsGroup = rnSrcDecls <=< fstM <=< findSplice . hsmodDecls . unLoc . pm_parsed_source
--   where fstM = return . fst
