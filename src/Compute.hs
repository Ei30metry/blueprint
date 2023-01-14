module Compute (toHsGroup) where


import           Control.Applicative       ()
import           Control.Monad             ( mapM, (<=<), (>=>) )
import           Control.Monad.Extra       ()
import           Control.Monad.Reader      ()
import           Control.Monad.Trans       ( MonadTrans, lift, liftIO )
import           Control.Monad.Trans.Class ()

import           Data.Foldable             ()
import qualified Data.Map                  as M
import           Data.Traversable          ()

import           GHC                       ( GhcMonad (getSession), GhcPs,
                                             GhcRn, HsGroup (..),
                                             HsModule (hsmodDecls), LHsDecl,
                                             LoadHowMuch (LoadAllTargets),
                                             Located, ModSummary (ms_hspp_opts),
                                             ParsedMod (..),
                                             ParsedModule (pm_mod_summary, pm_parsed_source),
                                             RenamedSource,
                                             TypecheckedModule (..),
                                             getModSummary, getSessionDynFlags,
                                             guessTarget, hscTypecheckRename,
                                             load, mkModuleName, parseModule,
                                             setSessionDynFlags, unLoc )
import           GHC.Driver.Env
import           GHC.Driver.Main           ( hscTypecheckRename )
import           GHC.Generics
import           GHC.Rename.Module         ( findSplice, rnSrcDecls )
import           GHC.Tc.Types              ( RnM, TcGblEnv (..), TcLclEnv (..),
                                             TcM, TcRn (..), TcRnIf (..) )


searchImplementation :: a
searchImplementation = undefined


-- NameSpace == Varname && NameSort == External ....
lookupName :: a
lookupName = undefined

-- toHsGroup = rnSrcDecls <=< fstM <=< findSplice . hsmodDecls . unLoc . pm_parsed_source
--   where fstM = return . fst
