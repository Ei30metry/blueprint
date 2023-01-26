module Main (main) where

import           App                    ( runBluePrint )

import           CLI                    ( parseSearchEnv )

import           Compute                ( rnWithGlobalEnv )

import           Control.Applicative    ( (<**>) )
import           Control.Monad.IO.Class ( liftIO )

import           GHC                    ( Backend (..), DynFlags (backend),
                                          GhcMonad (getSession),
                                          LoadHowMuch (LoadAllTargets),
                                          ParsedMod (parsedSource),
                                          TypecheckedMod (moduleInfo),
                                          TypecheckedModule (tm_renamed_source, tm_typechecked_source),
                                          desugarModule, getModSummary,
                                          getSessionDynFlags, guessTarget, load,
                                          mkModuleName, modInfoExports,
                                          modInfoTyThings, parseModule, runGhc,
                                          setSessionDynFlags, setTargets,
                                          typecheckModule )
import           GHC.Paths              ( libdir )
import           GHC.Utils.Outputable   ( showPprUnsafe )

import           Options.Applicative    ( execParser, fullDesc, header, helper,
                                          info, progDesc )

import           System.Environment     ( getArgs )


main :: IO ()
main = do
  searchEnv <- execParser (info (parseSearchEnv <**> helper)
                          (fullDesc <> progDesc "Print recursive declerations of an entity"
                           <> header "A different approach to showing outgoing call hierarchy for Haskell source code."))
  print searchEnv
-- main = runGhc (Just libdir) $ do
--   file <- liftIO $ head <$> getArgs
--   let fileModuleName = reverse $ takeWhile (/= '/') $ reverse $ take (length file - 3) file
--   env <- getSession
--   dflags <- getSessionDynFlags
--   setSessionDynFlags $ dflags { backend = NoBackend }

--   target <- guessTarget file Nothing
--   setTargets [target]
--   load LoadAllTargets
--   modSum <- getModSummary $ mkModuleName fileModuleName

--   pmod <- parseModule modSum
--   (glRdrEnv, rnSource) <- rnWithGlobalEnv pmod
--   -- let names =
--   liftIO $ putStrLn $ showPprUnsafe glRdrEnv
