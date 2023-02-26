
module Compute.Search where

import           App                      ()

import           Compute.AST              ()
import           Compute.Morphisms        ( occNameFromEntity )

import           Control.Lens.Combinators ( makeLenses )
import           Control.Lens.Getter      ()
import           Control.Lens.Lens        ()
import           Control.Lens.Operators   ()
import           Control.Lens.Setter      ()

import qualified Data.Map                 as M
import           Data.Tree                ()
import           Data.Tree.Lens           ()

import           GHC                      ( HsBindLR (..), HsValBinds, Pass,
                                            RenamedSource )
import           GHC.Types.Avail          ()
import           GHC.Types.Name           ( Name (..) )
import           GHC.Types.Name.Reader    ( GlobalRdrElt, GlobalRdrEnv,
                                            lookupGlobalRdrEnv )
import           GHC.Types.Name.Set       ()
import           GHC.Types.Unique         ( Uniquable (..) )

import           Types                    ( SearchEnv (..) )


data ASTEnv a = AEnv { _elemName     :: GlobalRdrElt
                     , _globalRdrEnv :: GlobalRdrEnv
                     , _renamedBinds :: HsValBinds a }

-- NOTE for finding the actual Name
-- NameSpace == Varname && NameSort == External ....
-- lookupName :: GlobalRdrEnv -> String -> [Name]


makeLenses ''ASTEnv

recurseImplementation :: Monad m => (GlobalRdrEnv, RenamedSource) -> m [a]
recurseImplementation = undefined


searchOccName :: Monad m => SearchEnv -> GlobalRdrEnv -> m [GlobalRdrElt]
searchOccName sEnv rdrEnv = return $ lookupGlobalRdrEnv rdrEnv (occNameFromEntity . entity $ sEnv)
