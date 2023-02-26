module Main where

import           App                    ( runBluePrint )

import           Blueprint              ( initializeGhc, prototypeFunc, rnTest )

import           CLI                    ( parseSearchEnv )

import           Compute                ( parseSourceFile )

import           Control.Applicative    ( (<**>) )
import           Control.Monad          ( replicateM_ )
import           Control.Monad.IO.Class ( liftIO )

import           Data.IORef

import           GHC                    ( ParsedModule (pm_parsed_source),
                                          runGhcT, unLoc )
import           GHC.Paths              ( libdir )
import           GHC.Utils.Outputable   ( showPprUnsafe )

import           Options.Applicative    ( execParser, fullDesc, header, helper,
                                          info, progDesc )

import           Types                  ( SearchEnv (..) )



main :: IO ()
main = runGhcT (Just libdir) $ do
  sEnv <- liftIO getSearchEnv
  let ent = entity sEnv
  let filePath = modPath sEnv
  modSum <- initializeGhc filePath
  (result, _) <- runBluePrint (rnTest @String ent) modSum
  -- liftIO . print . showPprUnsafe . unLoc . pm_parsed_source $ result
  liftIO . print . showPprUnsafe $ result

-- main :: IO ()
-- main = getSearchEnv >>= print


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
