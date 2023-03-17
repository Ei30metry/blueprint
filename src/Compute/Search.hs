module Compute.Search where

import           App                        ( BluePrint (..) )

import           Compute.AST                ( BluePrintAST (..) )
import           Compute.Morphisms          ( entityToName, occNameFromEntity )

import           Control.Lens.Combinators   ( Bifunctor (bimap), makeLenses )
import           Control.Lens.Getter        ()
import           Control.Lens.Lens          ()
import           Control.Lens.Operators     ()
import           Control.Lens.Setter        ()
import           Control.Monad              ( join )
import           Control.Monad.Trans.Reader ( ask )

import           Data.Coerce                ( coerce )
import           Data.Foldable              ( find )
import           Data.Functor.Classes       ( eq1 )
import           Data.Tree                  ( Tree (..) )
import           Data.Tree.Lens             ()

import           GHC                        ( GhcMonad (..),
                                              HsGroup, moduleName,
                                              moduleUnit )
import           GHC.Data.OrdList           ( fromOL )
import           GHC.Plugins                ( nonDetEltsUniqSet, sizeUniqSet )
import           GHC.Types.Avail            ()
import           GHC.Types.Name             ( Name (..) )
import           GHC.Types.Name.Reader      ( GlobalRdrElt, GlobalRdrEnv,
                                              lookupGlobalRdrEnv )
import           GHC.Types.Name.Set         ( DefUses, Defs,
                                              Uses )

import           Types                      ( Entity (..), SearchEnv (..) )


data CompEnv a = CompEnv { _elemName     :: GlobalRdrElt
                         , _globalRdrEnv :: GlobalRdrEnv
                         , _renamedBinds :: HsGroup a }

makeLenses ''CompEnv


searchOccName :: Monad m => SearchEnv -> GlobalRdrEnv -> m [GlobalRdrElt]
searchOccName sEnv rdrEnv = return $ lookupGlobalRdrEnv rdrEnv (occNameFromEntity . entity $ sEnv)

-- NOTE Unsafe Type Synonym for telling the user that the NameSet should have 1 and only 1 element in it
type Def = Defs

buildUsageTree :: Name -> [(Def, Uses)] -> Maybe (BluePrintAST Name)
buildUsageTree name [] = Just (pure name)
buildUsageTree name uses = coerce $ go name defUses
  where defUses = bimap (head . nonDetEltsUniqSet) nonDetEltsUniqSet <$> uses
        go :: Name -> [(Name, [Name])] -> Maybe (Tree Name)
        go root [] = Just (pure root)
        go root defs = join (case find ((== root) . fst) defs of
           Nothing    -> return $ Just (pure root)
           Just (d,u) -> return $ Node d <$> traverse (`go` defs) u)


buildTypeUsageAST = undefined
buildFunctionUsageAST = undefined

buildUsageAST = undefined

-- TODO fix the function to use GlobalRdrEnv
searchInDefUses :: forall w m. (GhcMonad m, Monoid w) => DefUses -> BluePrint (GlobalRdrEnv, Entity) w m (Maybe (BluePrintAST Name))
searchInDefUses defUses = BT $ do
    let definitions = toBinds $ fromOL defUses
    (gblEnv, toSearchFor) <- ask
    let name = entityToName toSearchFor gblEnv
    case name of
      Nothing -> return Nothing
      Just n -> return (buildUsageTree n =<< mapM helper definitions)
  where
    toBinds = filter (\x -> fmap sizeUniqSet (fst x) `eq1` Just 1)
    helper (Nothing, _) = Nothing
    helper (Just x, y)  = Just (x, y)
