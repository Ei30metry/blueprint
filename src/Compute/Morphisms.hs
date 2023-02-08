-- this modules purpose is to serve as a straightforward way to convert
-- GHC intenral types to blueprint types.

module Compute.Morphisms where

import           GHC.Types.Name        ( OccName, mkOccName, tcName, varName )
import           GHC.Types.Name.Reader ( GlobalRdrElt (..), GlobalRdrEnv (..),
                                         Parent (..) )

import           Types                 ( Entity (..), EntityOccDef, Func,
                                         Scope (..), SearchEnv (..) )

scopeParent :: Scope -> GHC.Types.Name.Reader.Parent
scopeParent (ParentS f _) = GHC.Types.Name.Reader.ParentIs undefined -- OccName mkNameSpace $ mkFastString f
scopeParent (TopLevel _)  = GHC.Types.Name.Reader.NoParent


entityToName :: Entity -> Maybe GHC.Types.Name.Reader.GlobalRdrElt
entityToName = undefined


occNameFromEntity :: Entity -> OccName
occNameFromEntity (DataTypeE t)   = mkOccName tcName $ typeName t
occNameFromEntity (FunctionE s _) = mkOccName varName $ funcOccString s


entityParent :: Entity -> GHC.Types.Name.Reader.Parent
entityParent (FunctionE s _) = scopeParent s
entityParent _               = GHC.Types.Name.Reader.NoParent

typeName :: a
typeName = undefined


funcOccString :: Scope -> Func
funcOccString (TopLevel func)   = func
funcOccString (ParentS _ lFunc) = lFunc

getEntityOccString :: Entity -> EntityOccDef
getEntityOccString (DataTypeE t)   = typeName t
getEntityOccString (FunctionE s _) = funcOccString s
