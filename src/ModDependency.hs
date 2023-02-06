module ModDependency where

import           Control.Monad              ( liftM, mapM, (<=<), (>=>) )
import           Control.Monad.IO.Class     ( MonadIO, liftIO )

import           GHC                        ( DynFlags )
import           GHC.Paths                  ()
import           GHC.Unit.Env
import           GHC.Unit.Home.ModInfo
import           GHC.Unit.Module
import           GHC.Unit.Module.Imported
import           GHC.Unit.Module.ModSummary
import           GHC.Unit.Module.Name

-- TODO return a ModSummary of only the modules you care about (locally defined modules)
-- Non-external packages

dynFlagToHomeUnitModule :: DynFlags -> Module
dynFlagToHomeUnitModule = undefined
