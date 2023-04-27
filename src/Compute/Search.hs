module Compute.Search where

import           App                        ( BluePrint (..), runBluePrint )

import           Compute.Morphisms          ( entityToName, occNameFromEntity )

import           Control.Lens.Combinators   ( Bifunctor (bimap), makeLenses )
import           Control.Lens.Getter        ()
import           Control.Lens.Lens          ()
import           Control.Lens.Operators     ()
import           Control.Lens.Setter        ()
import           Control.Monad              ( filterM, join, (<=<) )
import           Control.Monad.Except       ( MonadTrans (lift), join,
                                              runExcept )
import           Control.Monad.Trans
import           Control.Monad.Trans        ( lift )
import           Control.Monad.Trans.Except ( ExceptT, catchE, throwE, except )
import           Control.Monad.Trans.Reader ( ask )

import           Data.Bifunctor             ( Bifunctor (first) )
import           Data.Coerce                ( coerce )
import           Data.Foldable              ( Foldable (toList), find )
import           Data.Functor.Classes       ( eq1 )
import qualified Data.IntMap.Lazy           as IM
import           Data.Maybe                 ( fromJust, fromMaybe )
import           Data.Traversable           ( traverse )
import           Data.Tree                  ( Forest, Tree (..), levels,
                                              unfoldTree, unfoldTreeM )
import           Data.Tree.Lens             ()

import           GHC                        ( GhcMonad (..), HsBindLR (..),
                                              HsGroup, HsValBinds, Module,
                                              ModuleName, Pass, RenamedSource,
                                              isExternalName, moduleName,
                                              moduleUnit, nameModule )
import           GHC.Data.OrdList           ( fromOL )
import           GHC.Plugins                ( FastString (FastString),
                                              isInternalName, isWiredInName,
                                              lookupUniqSet, nameModule,
                                              nonDetEltsUniqSet,
                                              nonDetKeysUniqSet, sizeUniqSet,
                                              unpackFS )
import           GHC.Types.Avail            ()
import           GHC.Types.Name             ( Name (..) )
import           GHC.Types.Name.Reader      ( GlobalRdrElt, GlobalRdrEnv,
                                              lookupGlobalRdrEnv )
import           GHC.Types.Name.Set         ( DefUse, DefUses, Defs, NameSet,
                                              Uses )
import           GHC.Types.Unique           ( Uniquable (..) )
import           GHC.Types.Unique.Set       ( UniqSet )
import           GHC.Unit                   ( homeUnitAsUnit )

import           Types                      ( Entity (..), SearchEnv (..) )
import           Types.AST                  ( BluePrintAST (..) )


data CompEnv a = CompEnv { _elemName     :: GlobalRdrElt
                         , _globalRdrEnv :: GlobalRdrEnv
                         , _renamedBinds :: HsGroup a }

makeLenses ''CompEnv


searchOccName :: Monad m => SearchEnv -> GlobalRdrEnv -> m [GlobalRdrElt]
searchOccName sEnv rdrEnv = return $ lookupGlobalRdrEnv rdrEnv (occNameFromEntity . entity $ sEnv)

-- Unsafe Type Synonym for telling the user that the NameSet should have 1 and only 1 element in it
type Def = Defs


buildUsageTree :: Name -> [(Def, Uses)] -> Either String (BluePrintAST Name)
buildUsageTree name [] = Right (pure name)
buildUsageTree name uses = coerce $ go name defUses
  where defUses = bimap (head . nonDetEltsUniqSet) nonDetEltsUniqSet <$> uses
        go :: Name -> [(Name, [Name])] -> Either String (Tree Name)
        go root [] = Right (pure root)
        go root defs = join (case find ((== root) . fst) defs of
           Nothing    -> return $ Right (pure root)
           Just (d,u) -> return $ Node d <$> traverse (`go` defs) u)

-- FIXME use a monad transformer instead of nested expressions
searchInDefUses :: forall w m. (GhcMonad m, Monoid w) => DefUses -> BluePrint String (GlobalRdrEnv, Entity) w m (BluePrintAST Name)
searchInDefUses defUses = BT $ do
    let definitions = toBinds $ fromOL defUses
    (gblEnv, toSearchFor) <- ask
    let name = entityToName toSearchFor gblEnv
    case name of
      Left e -> lift (throwE e)
      Right n  -> case buildUsageTree n =<< mapM helper definitions of
        Right x  -> return x
        Left err -> lift $ throwE err
  where
    toBinds = filter (\x -> fmap sizeUniqSet (fst x) `eq1` Just 1)
    helper (Nothing, _) = Left "couldn't find"
    helper (Just x, y)  = Right (x, y)

buildTypeUsageAST = undefined
buildFunctionUsageAST = undefined
buildUsageAST = undefined


-- searchInDefUses' :: forall w m. (GhcMonad m, Monoid w) => DefUses -> BluePrint String (GlobalRdrEnv, Entity) w m (BluePrintAST Name)
-- searchInDefUses' defUses = BT $ do
--     let definitions = toBinds $ fromOL defUses
--     (gblEnv, toSearchFor) <- ask
--     lift . lift $ do
--       n <- except $ entityToName toSearchFor gblEnv
--       buildUsageTree n =<< mapM helper definitions
--   where
--     toBinds = filter (\x -> fmap sizeUniqSet (fst x) `eq1` Just 1)
--     helper (Nothing, _) = Left "couldn't find"
--     helper (Just x, y)  = Right (x, y)
