module Main where

import           App                    ( runBluePrint )

import GHC.Types.Name.Reader (pprGlobalRdrEnv)
import           Blueprint              ( initializeGhc, prototypeFunc, rnTest,
                                          seeFromTcGblEnv, parseSourceFile' )

import           CLI                    ( parseSearchEnv )

import           Compute                ( parseSourceFile, rnWithGlobalEnv', entityToGlbRdrElt )

import           Control.Applicative    ( (<**>) )
import           Control.Monad          ( replicateM_ , (<=<))
import           Control.Monad.IO.Class ( liftIO )

import           Data.Bifunctor         ( first )
import           Data.IORef
import           Data.Maybe             ( fromJust )

import           GHC                    ( ParsedModule (pm_parsed_source),
                                          runGhcT, unLoc, LoadHowMuch (LoadAllTargets), parseModule )
import           GHC.Data.OrdList       ( fromOL )
import           GHC.Paths              ( libdir )
import           GHC.Tc.Types           ( TcGblEnv (..) )
import           GHC.Utils.Outputable   ( SDoc (..), defaultDumpStyle,
                                          defaultSDocContext, defaultUserStyle,
                                          ppr, printSDocLn, showPprUnsafe, printSDoc )
import           GHC.Utils.Ppr          ( Mode (PageMode), style )

import           Options.Applicative    ( execParser, fullDesc, header, helper,
                                          info, progDesc )

import           Result                 ( banner )

import           System.IO              ( stdout )

import           Types                  ( SearchEnv (..) )
import GHC.Tc.Utils.Monad (readTcRef)


main :: IO ()
main = runner1

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

runner3 :: IO ()
runner3 = runGhcT (Just libdir) $ do
  sEnv <- liftIO getSearchEnv
  let ent = entity sEnv
  let filePath = modPath sEnv
  modSum <- initializeGhc filePath
  parsed <- parseModule modSum
  gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
  liftIO . defPrint . ppr $ entityToGlbRdrElt ent gblEnv
  liftIO . defPrint $ pprGlobalRdrEnv True gblEnv




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
