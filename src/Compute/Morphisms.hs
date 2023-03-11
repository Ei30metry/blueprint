-- this module's purpose is to serve as a straightforward way to convert
-- GHC intenral types to blueprint types.

module Compute.Morphisms where

import           App                        ( BluePrint (..) )

import           Control.Monad              ( filterM, (<=<), join )
import           Control.Monad.Trans        ( lift )
import           Control.Monad.Trans.Reader ( ask, asks )

import           Data.Foldable              ( find )

import           GHC.Driver.Env             ( HscEnv (hsc_unit_env) )
import           GHC.Driver.Monad           ( GhcMonad (getSession) )
import           GHC.Types.Avail            ( AvailInfo (..), Avails (..) )
import           GHC.Types.Name             ( Name, OccName, mkOccName, tcName,
                                              varName )
import           GHC.Types.Name.Occurrence  ( lookupOccEnv )
import           GHC.Types.Name.Reader      ( GlobalRdrElt (..),
                                              GlobalRdrEnv (..),
                                              ImpDeclSpec (is_mod),
                                              ImportSpec (is_decl),
                                              LocalRdrEnv (..), Parent (..),
                                              lookupGRE_Name_OccName,
                                              lookupGlobalRdrEnv,
                                              lookupLocalRdrOcc )
import           GHC.Unit.Env               ( UnitEnv (ue_home_unit) )
import           GHC.Unit.Home              ( HomeUnit )

import           Types                      ( Entity (..), EntityOccDef, Func,
                                              Scope (..), SearchEnv (..),
                                              TypeC (..) )
import GHC (ModuleName)


-- This function should only be used to search the entity gathered from command line
entityToGlbRdrElt :: Entity -> GlobalRdrEnv -> Maybe GlobalRdrElt
entityToGlbRdrElt ent env = find gre_lcl $ lookupGlobalRdrEnv env (occNameFromEntity ent)

-- entityToName' :: forall w m. (GhcMonad m, Monoid w) => Entity -> BluePrint (ModuleName, GlobalRdrEnv) w m (Maybe GlobalRdrElt)
-- entityToName' ent = BT $ do
--      (modName, gblEnv) <- ask
--      homeUnit <- lift . lift $ ue_home_unit . hsc_unit_env <$> getSession
--      let glbRdrElts = entityToGlbRdrElts ent gblEnv
--      -- let moduleNames = mconcat $ fmap (fmap (is_mod . is_decl ) . gre_imp) glbRdrElts
--      return undefined -- find (isFromCurrentMod modName) <$> glbRdrElts
--   where
--     isFromCurrentMod :: ModuleName -> GlobalRdrElt -> Bool
--     -- fmap ((is_mod . is_decl) . gre_imp)
--     isFromCurrentMod modName elt = undefined -- fmap (is_mod . is_decl) $ gre_imp elt == modName -- fmap ((is_mod . is_decl) . gre_imp) elt == modName

occNameFromEntity :: Entity -> OccName
occNameFromEntity (DataTypeE t)   = mkOccName tcName $ typeName t
occNameFromEntity (FunctionE s _) = mkOccName varName $ funcOccString s


funcOccString :: Scope -> Func
funcOccString (TopLevel func)   = func
funcOccString (ParentS _ lFunc) = lFunc

getEntityOccString :: Entity -> EntityOccDef
getEntityOccString (DataTypeE t)   = typeName t
getEntityOccString (FunctionE s _) = funcOccString s
