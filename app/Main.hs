module Main where

import           App                    ( runBluePrint )

import           Blueprint              ( initializeGhc, parseSourceFile',
                                          prototypeFunc, rnTest,
                                          seeFromTcGblEnv )

import           CLI                    ( parseSearchEnv )

import           Compute                ( entityToGlbRdrElt, parseSourceFile,
                                          rnWithGlobalEnv' )

import           Control.Applicative    ( (<**>) )
import           Control.Monad          ( guard, replicateM_, (<=<) )
import           Control.Monad.IO.Class ( liftIO )

import           Data.Bifunctor         ( first )
import           Data.IORef
import           Data.Maybe             ( fromJust )

import           GHC                    ( GhcMonad (getSession),
                                          LoadHowMuch (LoadAllTargets), Located,
                                          ModSummary (ms_textual_imps, ms_mod),
                                          ModuleName,
                                          ParsedModule (pm_parsed_source),
                                          getModuleGraph, getModuleInfo,
                                          parseModule, runGhcT, unLoc, mgModSummaries, moduleUnit)
import           GHC.Data.FastString    ( FastString )
import           GHC.Data.OrdList       ( fromOL )
import           GHC.Driver.Env         ( HscEnv (hsc_mod_graph, hsc_unit_env) )
import           GHC.Paths              ( libdir )
import           GHC.Tc.Types           ( TcGblEnv (..) )
import           GHC.Tc.Utils.Monad     ( readTcRef )
import           GHC.Types.Name.Reader  ( GlobalRdrElt (gre_imp, gre_lcl),
                                          ImpDeclSpec (is_mod),
                                          ImportSpec (is_decl, is_item),
                                          importSpecLoc, importSpecModule,
                                          pprGlobalRdrEnv )
import           GHC.Unit.Env           ( ue_home_unit )
import           GHC.Unit.Home          ( homeUnitId )
import           GHC.Utils.Outputable   ( SDoc (..), defaultDumpStyle,
                                          defaultSDocContext, defaultUserStyle,
                                          ppr, printSDoc, printSDocLn,
                                          showPprUnsafe )
import           GHC.Utils.Ppr          ( Mode (PageMode), style )

import           Options.Applicative    ( execParser, fullDesc, header, helper,
                                          info, progDesc )

import           Result                 ( banner )

import           System.IO              ( stdout )

import           Types                  ( SearchEnv (..) )


main :: IO ()
main = runner3


runner1 :: IO ()
runner1 = runGhcT (Just libdir) $ do
  sEnv <- liftIO getSearchEnv
  let ent = entity sEnv
  let filePath = modPath sEnv
  modSum <- initializeGhc filePath
  (result, _) <- runBluePrint (seeFromTcGblEnv @String tcg_dus) modSum
  (result2, _) <- runBluePrint (seeFromTcGblEnv @String tcg_used_gres) modSum
  fResult2 <- liftIO $ readIORef result2
  liftIO . printSDocLn defaultSDocContext (PageMode True) stdout . ppr $ result
  -- liftIO . printSDocLn defaultSDocContext (PageMode True) stdout . ppr $ fResult2


defPrint :: SDoc -> IO ()
defPrint = printSDocLn defaultSDocContext (PageMode True) stdout


runner2 :: IO ()
runner2 = runGhcT (Just libdir) $ do
  let path = "/Users/artin/Programming/projects/blueprint/test/golden/Golden2.hs"
  parsed <- parseSourceFile' LoadAllTargets path
  gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
  liftIO . defPrint $ pprGlobalRdrEnv True gblEnv

-- testFunc :: ModSummary ->

runner3 :: IO ()
runner3 = runGhcT (Just libdir) $ do
    sEnv <- liftIO getSearchEnv
    let ent = entity sEnv
    let filePath = modPath sEnv
    modSum <- initializeGhc filePath
    parsed <- parseModule modSum
    gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
    let glbRdrElt = entityToGlbRdrElt ent gblEnv
    let modNames = fmap (fmap (is_mod . is_decl) . gre_imp) glbRdrElt
    modSummaries <- mgModSummaries <$> getModuleGraph
    hsc <- getSession
    liftIO . defPrint $ ppr glbRdrElt
  -- liftIO . defPrint $ pprGlobalRdrEnv True gblEnv


printBindings :: FilePath -> IO ()
printBindings filePath = runGhcT (Just libdir) $ do
  result <- prototypeFunc filePath
  liftIO . print $ showPprUnsafe result


printGatheredSEnv :: IO ()
printGatheredSEnv = commandLineInterface >>= print


commandLineInterface :: IO SearchEnv
commandLineInterface = do
  execParser (info (parseSearchEnv <**> helper)
                          (fullDesc <> progDesc "Print recursive declerations of an entity"
                           <> header "A different approach to showing outgoing call hierarchy for Haskell source code."))

getSearchEnv :: IO SearchEnv
getSearchEnv = commandLineInterface
