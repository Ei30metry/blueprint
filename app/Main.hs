module Main where

import           App                    ( runBluePrint )

import           Blueprint              ( initializeGhc, prototypeFunc, rnTest,
                                          seeFromTcGblEnv)

import           CLI                    ( parseSearchEnv )

import           Compute                ( parseSourceFile )

import           Control.Applicative    ( (<**>) )
import           Control.Monad          ( replicateM_ )
import           Control.Monad.IO.Class ( liftIO )

import           Data.IORef

import           GHC                    ( ParsedModule (pm_parsed_source),
                                          runGhcT, unLoc )
import           GHC.Data.OrdList       ( fromOL )
import           GHC.Paths              ( libdir )
import           GHC.Utils.Outputable   ( SDoc (..), defaultDumpStyle,
                                          defaultSDocContext, defaultUserStyle,
                                          ppr, printSDocLn, showPprUnsafe )
import           GHC.Utils.Ppr          ( style, Mode (PageMode) )

import           Options.Applicative    ( execParser, fullDesc, header, helper,
                                          info, progDesc )

import           System.IO              ( stdout )

import           Types                  ( SearchEnv (..) )
import Result (banner)
import GHC.Tc.Types (TcGblEnv(..))



main :: IO ()
main = runGhcT (Just libdir) $ do
  sEnv <- liftIO getSearchEnv
  let ent = entity sEnv
  let filePath = modPath sEnv
  modSum <- initializeGhc filePath
  (result, _) <- runBluePrint (seeFromTcGblEnv @String tcg_dus) modSum
  (result2, _) <- runBluePrint (seeFromTcGblEnv @String tcg_used_gres ) modSum
  result' <- liftIO $ readIORef result2
  -- liftIO . print . showPprUnsafe . unLoc . pm_parsed_source $ result
  liftIO . printSDocLn defaultSDocContext (PageMode True) stdout . ppr $ result
  liftIO . putStrLn $ banner "tcg_used_gres"
  liftIO $ mapM_ (printSDocLn defaultSDocContext (PageMode True) stdout . ppr) result'


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
