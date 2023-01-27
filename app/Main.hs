module Main (main) where

import           CLI                    ( parseSearchEnv )

import           Compute                ( occNameFromEntity, rnWithGlobalEnv )

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
import           GHC.Types.Name.Reader  ( GlobalRdrElt, GlobalRdrEnv,
                                          lookupGlobalRdrEnv )
import           GHC.Utils.Outputable   ( showPprUnsafe )

import           Options.Applicative    ( execParser, fullDesc, header, helper,
                                          info, progDesc )

import           Types                  ( SearchEnv (..) )



main :: IO ()
main = printGatheredSEnv
-- main = do
--   sEnv <- getSearchEnv
--   glblRdrEnv <- sourceToGlobalRdrEnv (modPath sEnv)
--   result <- searchOccName sEnv glblRdrEnv
--   print $ showPprUnsafe result


-- TODO not all module names are the same as their file name, give users option to set the style
mkFileModName :: FilePath -> String
mkFileModName = reverse . takeWhile (/= '/') . reverse . (\fp -> take (length fp - 3) fp)


printGatheredSEnv :: IO ()
printGatheredSEnv = commandLineInterface >>= print


commandLineInterface :: IO SearchEnv
commandLineInterface = do
  execParser (info (parseSearchEnv <**> helper)
                          (fullDesc <> progDesc "Print recursive declerations of an entity"
                           <> header "A different approach to showing outgoing call hierarchy for Haskell source code."))

getSearchEnv :: IO SearchEnv
getSearchEnv = commandLineInterface


searchOccName :: SearchEnv -> GlobalRdrEnv -> IO [GlobalRdrElt]
searchOccName sEnv rdrEnv = return $ lookupGlobalRdrEnv rdrEnv (occNameFromEntity . entity $ sEnv)


sourceToGlobalRdrEnv :: FilePath -> IO GlobalRdrEnv
sourceToGlobalRdrEnv filePath = runGhc (Just libdir) $ do
  let fileModuleName = mkFileModName filePath
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { backend = NoBackend }

  target <- guessTarget filePath Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName fileModuleName

  pmod <- parseModule modSum
  (glRdrEnv, _) <- rnWithGlobalEnv pmod
  return glRdrEnv
