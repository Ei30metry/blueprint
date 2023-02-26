-- this module's purpose is to serve as a straightforward way to convert
-- GHC intenral types to blueprint types.

module Compute.Morphisms where

import           GHC.Types.Avail           ( AvailInfo (..), Avails (..) )
import           GHC.Types.Name            ( OccName, mkOccName, tcName,
                                             varName )
import           GHC.Types.Name.Occurrence ( lookupOccEnv )
import           GHC.Types.Name.Reader     ( GlobalRdrElt (..),
                                             GlobalRdrEnv (..),
                                             LocalRdrEnv (..), Parent (..),
                                             lookupLocalRdrOcc, lookupGRE_Name_OccName, lookupGlobalRdrEnv )

import Control.Monad.Trans.Reader (asks, ask)
import           Types                     ( Entity (..), EntityOccDef, Func,
                                             Scope (..), SearchEnv (..), TypeC (..) )

-- scopeToParent :: Scope -> LocalRdrEnv -> Parent
-- scopeToParent (ParentS f _) env = ParentIs $ OccName mkNameSpace $ mkFastString f
-- scopeToParent (TopLevel _)  _ = NoParent


-- entityToGlobalRdrElt :: SearchEnv -> Maybe GlobalRdrElt
-- entityToGlobalRdrElt (DataTypeE t)   = undefined
-- entityToGlobalRdrElt (FunctionE s _) = undefined

--TODO check for WiredIn, External and Internal NameSorts
-- entityToGlobalRdrElt :: BluePrint Entity w m (Maybe GlobalRdrElt)
-- entityToGlobalRdrElt = do

-- entityToGlbRdrElt :: Entity -> GlobalRdrEnv -> Maybe GlobalRdrElt
entityToGlbRdrElt :: Entity -> GlobalRdrEnv -> [GlobalRdrElt]
entityToGlbRdrElt ent env = lookupGlobalRdrEnv env (occNameFromEntity ent)

occNameFromEntity :: Entity -> OccName
occNameFromEntity (DataTypeE t)   = mkOccName tcName $ typeName t
occNameFromEntity (FunctionE s _) = mkOccName varName $ funcOccString s


funcOccString :: Scope -> Func
funcOccString (TopLevel func)   = func
funcOccString (ParentS _ lFunc) = lFunc

getEntityOccString :: Entity -> EntityOccDef
getEntityOccString (DataTypeE t)   = typeName t
getEntityOccString (FunctionE s _) = funcOccString s
