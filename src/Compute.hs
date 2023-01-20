module Compute (rnWithGlobalEnv) where


import           Control.Applicative        ()
import           Control.Monad              ( join, mapM, (<=<), (>=>) )
import           Control.Monad.Extra        ()
import           Control.Monad.Trans        ( MonadTrans, lift, liftIO )
import           Control.Monad.Trans.Class  ()
import           Control.Monad.Trans.Reader ( ReaderT, ask, asks, local,
                                              mapReaderT, withReaderT )

import           Data.Foldable              ()
import           Data.IntMap.Lazy           ( lookup )
import qualified Data.Map                   as M
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

searchImplementation :: a
searchImplementation = undefined


rnWithGlobalEnv :: GhcMonad m => ParsedModule -> m (GlobalRdrEnv, Maybe RenamedSource)
rnWithGlobalEnv = return . glbWithRenamed <=< typecheckModule
  where glbWithRenamed tChecked = (tcg_rdr_env . fst . tm_internals_$ tChecked, tm_renamed_source tChecked)



-- NameSpace == Varname && NameSort == External ....
-- lookupName :: GlobalRdrEnv -> String -> [Name]
lookupName :: ReaderT GlobalRdrEnv Maybe Name -> String -> Maybe Name
lookupName = undefined

lookupManyNames :: [String] -> [Name]
lookupManyNames = undefined

-- toHsGroup = rnSrcDecls <=< fstM <=< findSplice . hsmodDecls . unLoc . pm_parsed_source
--   where fstM = return . fst
