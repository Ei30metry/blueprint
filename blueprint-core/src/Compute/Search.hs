module Compute.Search where

import           App                        ( BluePrint (..) )

import           Compute.Morphisms          ( entityToName, occNameFromEntity )
import           Control.Lens.Combinators   ( Bifunctor (bimap), makeLenses )
import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Operators
import           Control.Lens.Setter
import           Control.Monad              ( join )
import           Control.Monad.Trans
import           Control.Monad.Trans.Except ( except )
import           Control.Monad.Trans.Reader ( ask )

import           Data.Coerce                ( coerce )
import           Data.Foldable              ( find )
import           Data.Functor.Classes       ( eq1 )
import           Data.Maybe                 ( fromJust )
import           Data.Tree                  ( Tree (..) )
import           Data.Tree.Lens             ()

import           GHC                        ( ClsInst, FamInst, GhcMonad (..),
                                              GhcPass (GhcRn), GhcPs, GhcRn,
                                              GhcTc, HsGroup, LHsBinds, LHsDecl,
                                              Module, TyCon, moduleName,
                                              moduleUnit )
import           GHC.Core.FamInstEnv        ( FamInstEnv )
import           GHC.Core.InstEnv           ( InstEnv )
import           GHC.Core.PatSyn            ( PatSyn )
import           GHC.Data.OrdList           ( fromOL )
import           GHC.Plugins                ( AnnEnv, nonDetEltsUniqSet,
                                              sizeUniqSet )
import           GHC.Tc.Types               ( ImportAvails, TcGblEnv (..) )
import           GHC.Types.Avail            ()
import           GHC.Types.Name             ( Name (..) )
import           GHC.Types.Name.Reader      ( GlobalRdrElt, GlobalRdrEnv,
                                              lookupGlobalRdrEnv )
import           GHC.Types.Name.Set         ( DefUses, Defs, NameSet, Uses )
import           GHC.Types.TypeEnv          ( TypeEnv )

import           Types                      ( Entity (..), SearchEnv (..) )
import           Types.AST                  ( BluePrintAST (..) )
import           Types.Error


-- TODO experiment with st
data CompEnv a = CompEnv { _globalRdrEnv      :: GlobalRdrEnv
                         -- , _usedGREs          :: [GlobalRdrElt]
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

-- TODO see if you can use ST for better performance and compatibitly with the current
-- typechecker API
initCompEnv :: TcGblEnv -> Either CompEnvError (CompEnv GhcRn)
initCompEnv (tcg_rn_decls -> Nothing) = Left RnDeclError
initCompEnv TcGblEnv{..} = Right $ CompEnv { _globalRdrEnv = tcg_rdr_env
                                           -- , _usedGREs = unTCRef tcg_used_gres
                                           , _renamedDecls = fromJust tcg_rn_decls
                                           , _currentModule = tcg_mod
                                           , _typeEnv = tcg_type_env
                                           , _annotationEnv = tcg_ann_env
                                           , _instanceEnv = tcg_inst_env
                                           , _topInstances = tcg_insts
                                           , _defUses = tcg_dus
                                           , _familyInstanceEnv = tcg_fam_inst_env
                                           , _familyInstances = tcg_fam_insts
                                           , _imports = tcg_imports
                                           , _patternSynonyms = tcg_patsyns
                                           , _topTyCons = tcg_tcs}

searchOccName :: Monad m => SearchEnv -> GlobalRdrEnv -> m [GlobalRdrElt]
searchOccName sEnv rdrEnv = return $ lookupGlobalRdrEnv rdrEnv (occNameFromEntity . entity $ sEnv)


type Def = Defs

-- TODO refactor to keep Data types and classes
buildUsageTree :: Name -> [(Def, Uses)] -> Either String (BluePrintAST Name)
buildUsageTree name [] = Right (pure name)
buildUsageTree name uses = coerce @(Either String (Tree Name)) $ go name dfUses
  where dfUses = bimap (head . nonDetEltsUniqSet) nonDetEltsUniqSet <$> uses
        go root [] = Right (pure root)
        go root defs = join (case find ((== root) . fst) defs of
           Nothing    -> return $ Right (pure root)
           Just (d,u) -> return $ Node d <$> traverse (`go` defs) u)


buildUsageTree' :: Name -> [(Def, Uses)] -> Either String (BluePrintAST Name)
buildUsageTree' name [] = Right (pure name)
buildUsageTree' name uses = coerce $ go name dfUses
  where dfUses = bimap (head . nonDetEltsUniqSet) nonDetEltsUniqSet <$> uses
        go :: Name -> [(Name, [Name])] -> Either String (Tree Name)
        go root [] = Right (pure root)
        go root defs = join (case find ((== root) . fst) defs of
           Nothing    -> return $ Right (pure root)
           Just (d,u) -> return $ Node d <$> traverse (`go` defs) u)


-- TODO refactor to output Datatype Structures too
searchInDefUses :: forall w m. (GhcMonad m, Monoid w) => DefUses -> BluePrint String (GlobalRdrEnv, Entity) w m (BluePrintAST Name)
searchInDefUses dfUses = BT $ do
    let definitions = toBinds $ fromOL dfUses
    (gblEnv, toSearchFor) <- ask
    name <-  transform $ entityToName toSearchFor gblEnv
    transform $ buildUsageTree name =<< mapM helper definitions
  where
    toBinds = filter (\x -> fmap sizeUniqSet (fst x) `eq1` Just 1)
    helper (Nothing, _) = Left "couldn't find"
    helper (Just x, y)  = Right (x, y)
    transform = lift . except


buildTypeUsageAST = undefined
buildFunctionUsageAST = undefined
buildUsageAST = undefined
