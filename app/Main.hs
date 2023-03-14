module Main where

import           App                    ( runBluePrint )

import           Blueprint              ( initializeGhc, parseSourceFile',
                                          prototypeFunc, rnTest,
                                          seeFromTcGblEnv )

import           CLI                    ( parseSearchEnv )

import           Compute                ( BluePrintAST (..), entityToGlbRdrElt,
                                          entityToName, parseSourceFile,
                                          rnWithGlobalEnv', searchInDefUses )

import           Control.Applicative    ( (<**>) )
import           Control.Exception      ( asyncExceptionFromException )
import           Control.Monad          ( guard, replicateM_, (<=<) )
import           Control.Monad.IO.Class ( liftIO )

import           Data.Bifunctor         ( first )
import           Data.Coerce            ( coerce )
import           Data.Functor.Classes   ( eq1 )
import           Data.IORef
import           Data.Maybe             ( fromJust )
import           Data.Tree              ( drawTree )
import qualified Data.Tree              as T

import           GHC                    ( GhcMonad (getSession),
                                          LoadHowMuch (LoadAllTargets), Located,
                                          ModSummary (ms_mod, ms_textual_imps),
                                          ModuleName, Name,
                                          ParsedModule (pm_parsed_source),
                                          getModuleGraph, getModuleInfo,
                                          mgModSummaries, moduleUnit,
                                          parseModule, runGhcT, unLoc )
import           GHC.Data.FastString    ( FastString )
import           GHC.Data.OrdList       ( fromOL )
import           GHC.Driver.Env         ( HscEnv (hsc_mod_graph, hsc_unit_env) )
import           GHC.Paths              ( libdir )
import           GHC.Plugins            ( sizeUniqSet )
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

import           Result
import           Result                 ( banner )

import           System.IO              ( stdout )

import           Types                  ( SearchEnv (..) )


main :: IO ()
main = runner4


runner1 :: IO ()
runner1 = runGhcT (Just libdir) $ do
    sEnv <- liftIO getSearchEnv
    let ent = entity sEnv
    let filePath = modPath sEnv
    modSum <- initializeGhc filePath
    (result, _) <- runBluePrint (seeFromTcGblEnv @String tcg_dus) modSum
    (result2, _) <- runBluePrint (seeFromTcGblEnv @String tcg_used_gres) modSum
    fResult2 <- liftIO $ readIORef result2
    -- liftIO . defPrint . ppr $ result
    liftIO . defPrint . ppr $ toBinds $ fromOL result
  where
    toBinds = filter (\x -> fmap sizeUniqSet (fst x) `eq1` Just 1)
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

runner4 :: IO ()
runner4 = runGhcT (Just libdir) $ do
    sEnv <- liftIO getSearchEnv
    let ent = entity sEnv
    let filePath = modPath sEnv
    modSum <- initializeGhc filePath
    parsed <- parseModule modSum
    gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
    (result, _) <- runBluePrint (seeFromTcGblEnv @String tcg_dus) modSum
    (res, _) <- runBluePrint (searchInDefUses @String result) (gblEnv, ent)
    let name = entityToName ent gblEnv
    -- liftIO . defPrint . ppr $ T.levels . coerce @_ @(T.Tree Name) <$> res --(T.Tree Name)) <$> res
    liftIO . print . fmap nameTreeToStringTree $ res
    liftIO . print . fmap (drawTree . nameTreeToStringTree) $ res


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
