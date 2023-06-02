module Development.Blueprint.Compute.Search where

import           Control.Lens.Combinators                ( Bifunctor (bimap),
                                                           makeLenses )
import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Operators
import           Control.Lens.Setter
import           Control.Monad                           ( join )
import           Control.Monad.Trans
import           Control.Monad.Trans.Except              ( except )
import           Control.Monad.Trans.Reader              ( ask )

import           Data.Coerce                             ( coerce )
import           Data.Foldable                           ( find )
import           Data.Functor.Classes                    ( eq1 )
import           Data.Maybe                              ( fromJust )
import           Data.Tree                               ( Tree (..) )
import           Data.Tree.Lens                          ()

import           Development.Blueprint.Monad ( BluePrint (..) )
import           Development.Blueprint.Compute.Morphisms ( entityToName,
                                                           occNameFromEntity )
import           Development.Blueprint.Types             ( Entity (..),
                                                           SearchEnv (..) )
import           Development.Blueprint.Types.AST         ( BluePrintAST (..) )
import           Development.Blueprint.Types.Error

import           GHC                                     ( ClsInst, FamInst,
                                                           GhcMonad (..),
                                                           GhcPass (GhcRn),
                                                           GhcPs, GhcRn, GhcTc,
                                                           HsGroup, LHsBinds,
                                                           LHsDecl, Module,
                                                           TyCon, moduleName,
                                                           moduleUnit )
import           GHC.Core.FamInstEnv                     ( FamInstEnv )
import           GHC.Core.InstEnv                        ( InstEnv )
import           GHC.Core.PatSyn                         ( PatSyn )
import           GHC.Data.OrdList                        ( fromOL )
import           GHC.Plugins                             ( AnnEnv,
                                                           nonDetEltsUniqSet,
                                                           sizeUniqSet )
import           GHC.Tc.Types                            ( ImportAvails,
                                                           TcGblEnv (..) )
import           GHC.Types.Avail                         ()
import           GHC.Types.Name                          ( Name (..) )
import           GHC.Types.Name.Reader                   ( GlobalRdrElt,
                                                           GlobalRdrEnv,
                                                           lookupGlobalRdrEnv )
import           GHC.Types.Name.Set                      ( DefUses, Defs,
                                                           NameSet, Uses )
import           GHC.Types.TypeEnv                       ( TypeEnv )


-- TODO experiment with st


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


-- TODO refactor to output Datatype Structures too
searchInDefUses :: forall w m. (GhcMonad m, Monoid w) => DefUses -> BluePrint String (GlobalRdrEnv, Entity) w m (BluePrintAST Name)
searchInDefUses dfUses = do
    let definitions = toBinds $ fromOL dfUses
    (gblEnv, toSearchFor) <- ask
    name <-  transform $ entityToName toSearchFor gblEnv
    transform $ buildUsageTree name =<< mapM helper definitions
  where
    toBinds = filter (\x -> fmap sizeUniqSet (fst x) `eq1` Just 1)
    helper (Nothing, _) = Left "couldn't find"
    helper (Just x, y)  = Right (x, y)
    transform = lift . except
