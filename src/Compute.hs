module Compute where

import           Control.Monad.Except (ExceptT (..))
import Control.Monad.Trans.Class ()
import           Control.Monad.IO.Class            ( liftIO )

import           Data.Foldable                     ( find )
import           Data.Traversable                  ( traverse )

import           GHC                               ( DynFlags (..),
                                                     GenLocated (..), Ghc,
                                                     GhcMonad (..),
                                                     HsModule (..),
                                                     LoadHowMuch (..),
                                                     ParsedModule (..),
                                                     ParsedSource, addTarget,
                                                     depanal,
                                                     getSessionDynFlags,
                                                     guessTarget, load,
                                                     mgModSummaries,
                                                     parseModule,
                                                     setSessionDynFlags )
import           GHC.Driver.Ppr                    ( showSDoc )
import           GHC.Hs.Extension                  ( GhcPs (..) )
import           GHC.Parser.Annotation             ( SrcSpanAnnA )
import           GHC.Runtime.Loader
import           GHC.Types.Target                  ( Target (..) )
import           GHC.Unit.Module.ModSummary        ( msHsFilePath )
import           GHC.Utils.Outputable              ( ppr )
import           GHC.Utils.Panic                   ( panic )

import           Language.Haskell.Syntax.Decls     ( HsDecl, LHsDecl )
import           Language.Haskell.Syntax.Extension ( UnXRec (..), XRec (..) )


-- toAST :: GhcMonad m => FilePath -> m [HsDecl GhcPs]
toAST :: GhcMonad m => FilePath -> m [HsDecl GhcPs]
toAST fn = do
  target <- guessTarget fn Nothing
  addTarget target
  dflags <- getSessionDynFlags
  setSessionDynFlags dflags
  _ <- load LoadAllTargets
  modGraph <- depanal [] True
  case (find ((== fn) . msHsFilePath) (mgModSummaries modGraph)) of
    Just modSummary ->  map (\(L _ e) -> e) . hsmodDecls . (\(L _ e) -> e). pm_parsed_source <$> parseModule modSummary
    Nothing -> panic "target FilePath not found in module dependency graph"


testFilePath :: String
testFilePath = "~/Programming/practicing/Haskell/Blueprint-tests/main.hs"
