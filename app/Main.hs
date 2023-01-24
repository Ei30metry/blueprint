module Main (main) where

import           Compute                ( rnWithGlobalEnv )

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

import           System.Environment     ( getArgs )


main :: IO ()
main = runGhc (Just libdir) $ do
  file <- liftIO $ head <$> getArgs
  let fileModuleName = reverse $ takeWhile (/= '/') $ reverse $ take (length file - 3) file
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { backend = NoBackend }

  target <- guessTarget file Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName fileModuleName

  pmod <- parseModule modSum
  (glRdrEnv, rnSource) <- rnWithGlobalEnv pmod
  -- let names =
  liftIO $ putStrLn $ showPprUnsafe glRdrEnv
