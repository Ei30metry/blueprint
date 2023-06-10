-- this module's purpose is to serve as a straightforward way to convert
-- GHC intenral types to blueprint types.

module Development.Blueprint.Compute.Morphisms where


import           Control.Monad                   ( (<=<) )

import           Data.Foldable                   ( find )
import           Data.Text                       ( Text, unpack )

import           Development.Blueprint.Types     ( Entity (..), EntityOccDef,
                                                   Func, Scope (..),
                                                   TypeC (..) )
import           Development.Blueprint.Types.AST ( BluePrintAST )

import           GHC.Types.Avail                 ( GreName (NormalGreName) )
import           GHC.Types.Name                  ( Name, OccName, mkOccName,
                                                   tcName, varName )
import           GHC.Types.Name.Reader           ( GlobalRdrElt (..),
                                                   GlobalRdrEnv (..),
                                                   lookupGlobalRdrEnv )

-- FIXME: should work for both functions and types
getEntityOccString :: Entity -> EntityOccDef
getEntityOccString (SubstituteE s _) = s
getEntityOccString (ShowE s) = s

-- FIXME: should work for both functions and types
occNameFromEntity :: Entity -> OccName
occNameFromEntity (SubstituteE s _) = mkOccName varName . unpack $ s
occNameFromEntity (ShowE s) = mkOccName varName . unpack $ s

-- This function should only be used to search the entity gathered from command line
entityToGlbRdrElt :: Entity -> GlobalRdrEnv -> Either String GlobalRdrElt
entityToGlbRdrElt ent env = case find gre_lcl $ lookupGlobalRdrEnv env (occNameFromEntity ent) of
  Nothing  -> Left "Couldn't find the desierd GlobalRdrElt"
  Just elt -> Right elt


entityToName :: Entity -> GlobalRdrEnv -> Either String Name
entityToName ent = isNormalGreName <=< return . gre_name <=< entityToGlbRdrElt ent
  where isNormalGreName = \case
          NormalGreName name -> Right name
          _                  -> Left "couldn't find name"

astToTreeOutput :: forall a. BluePrintAST a -> Text
astToTreeOutput = undefined

astToSVGOutput :: forall a b. BluePrintAST a -> b
astToSVGOutput = undefined
