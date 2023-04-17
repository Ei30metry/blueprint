-- this module's purpose is to serve as a straightforward way to convert
-- GHC intenral types to blueprint types.

module Compute.Morphisms where


import           Control.Monad         ( (<=<) )

import           Data.Foldable         ( find )
import           Data.Text             ( Text, unpack )

import           GHC.Types.Avail       ( GreName (NormalGreName) )
import           GHC.Types.Name        ( Name, OccName, mkOccName, tcName,
                                         varName )
import           GHC.Types.Name.Reader ( GlobalRdrElt (..), GlobalRdrEnv (..),
                                         lookupGlobalRdrEnv )

import           Types                 ( Entity (..), EntityOccDef, Func,
                                         Scope (..), TypeC (..) )
import           Types.AST             ( BluePrintAST )

funcOccString :: Scope -> Func
funcOccString (TopLevel func)   = func
funcOccString (ParentS _ lFunc) = lFunc

getEntityOccString :: Entity -> EntityOccDef
getEntityOccString (DataTypeE t)   = typeName t
getEntityOccString (FunctionE s _) = funcOccString s

occNameFromEntity :: Entity -> OccName
occNameFromEntity (DataTypeE t)   = mkOccName tcName . unpack $ typeName t
occNameFromEntity (FunctionE s _) = mkOccName varName . unpack $ funcOccString s


-- This function should only be used to search the entity gathered from command line
entityToGlbRdrElt :: Entity -> GlobalRdrEnv -> Maybe GlobalRdrElt
entityToGlbRdrElt ent env = find gre_lcl $ lookupGlobalRdrEnv env (occNameFromEntity ent)


entityToName :: Entity -> GlobalRdrEnv -> Maybe Name
entityToName ent = isNormalGreName <=< return . gre_name <=< entityToGlbRdrElt ent
  where isNormalGreName = \case
          NormalGreName name -> Just name
          _                  -> Nothing

astToTreeOutput :: forall a. BluePrintAST a -> Text
astToTreeOutput = undefined

astToSVGOutput :: forall a b. BluePrintAST a -> b
astToSVGOutput = undefined
