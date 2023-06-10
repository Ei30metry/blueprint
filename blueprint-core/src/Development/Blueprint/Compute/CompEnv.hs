-- | Working with the Computation environment

module Development.Blueprint.Compute.CompEnv where


import           Control.Lens.Combinators          ( makeLenses )

import           Data.Maybe                        ( fromJust )
import           Data.Tree.Lens                    ()

import           Development.Blueprint.Types.Error ( CompEnvError (..) )

import           GHC                               ( ClsInst, FamInst, GhcRn,
                                                     HsGroup, LHsExpr, Module,
                                                     ParsedModule (..),
                                                     ParsedSource (..), TyCon )
import           GHC.Core.FamInstEnv               ( FamInstEnv )
import           GHC.Core.InstEnv                  ( InstEnv )
import           GHC.Core.PatSyn                   ( PatSyn )
import           GHC.Hs
import           GHC.Plugins                       ( AnnEnv )
import           GHC.Tc.Types                      ( ImportAvails,
                                                     TcGblEnv (..) )
import           GHC.Types.Name.Reader             ( GlobalRdrEnv )
import           GHC.Types.Name.Set                ( DefUses )
import           GHC.Types.TypeEnv                 ( TypeEnv )


data CompEnv a = CompEnv { _globalRdrEnv      :: GlobalRdrEnv
                         , _expr              :: LHsExpr a
                         , _renamedDecls      :: HsGroup a
                         , _currentModule     :: Module
                         , _typeEnv           :: TypeEnv
                         , _annotationEnv     :: AnnEnv
                         , _instanceEnv       :: InstEnv
                         , _topInstances      :: [ClsInst]
                         , _defUses           :: DefUses
                         , _familyInstances   :: [FamInst]
                         , _familyInstanceEnv :: FamInstEnv
                         , _imports           :: ImportAvails
                         , _patternSynonyms   :: [PatSyn]
                         , _topTyCons         :: [TyCon] }

makeLenses ''CompEnv

-- | Initialize a computation environment from TcGblEnv, and parsers AST
initCompEnv :: TcGblEnv -> Either CompEnvError (CompEnv GhcRn)
initCompEnv (tcg_rn_decls -> Nothing) = Left RnDeclError
initCompEnv TcGblEnv{..}             = Right
  $ CompEnv { _globalRdrEnv      = tcg_rdr_env
            , _renamedDecls      = fromJust tcg_rn_decls
            , _currentModule     = tcg_mod
            , _typeEnv           = tcg_type_env
            , _annotationEnv     = tcg_ann_env
            , _instanceEnv       = tcg_inst_env
            , _topInstances      = tcg_insts
            , _defUses           = tcg_dus
            , _familyInstanceEnv = tcg_fam_inst_env
            , _familyInstances   = tcg_fam_insts
            , _imports           = tcg_imports
            , _patternSynonyms   = tcg_patsyns
            , _topTyCons         = tcg_tcs
            }
