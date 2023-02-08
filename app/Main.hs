module Main (main) where

import           CLI                    ( parseSearchEnv )

import           Compute                ( mkFileModName, occNameFromEntity,
                                          parseSourceFile,
                                          renamedSourceToBindings,
                                          rnWithGlobalEnv, searchOccName )

import           Control.Applicative    ( (<**>) )
import           Control.Monad          ( (<=<), (>=>) )
import           Control.Monad.IO.Class ( MonadIO, liftIO )

import           Data.Functor           ( (<&>) )

import           GHC                    ( Backend (..), DynFlags (backend), Ghc,
                                          GhcMonad (getSession), GhcT,
                                          LoadHowMuch (LoadAllTargets),
                                          ParsedMod (parsedSource),
                                          ParsedModule, RenamedSource,
                                          TypecheckedMod (moduleInfo),
                                          TypecheckedModule (tm_renamed_source, tm_typechecked_source),
                                          depanal, desugarModule, getModSummary,
                                          getSessionDynFlags, guessTarget, load,
                                          mkModuleName, modInfoExports,
                                          modInfoTyThings, parseModule, runGhc,
                                          runGhcT, setSessionDynFlags,
                                          setTargets, typecheckModule )
import           GHC.Paths              ( libdir )
import           GHC.Utils.Outputable   ( showPprUnsafe )

import           Options.Applicative    ( execParser, fullDesc, header, helper,
                                          info, progDesc )

import           System.Environment     ( getArgs )

import           Types                  ( SearchEnv (..) )



-- TODO we should be able to compose and then runGhc at last
-- BUG changed the arity of parseSourceFile function, fix the code below
main :: IO ()
main = printGatheredSEnv

prototype :: IO ()
prototype = do
  sEnv <- getSearchEnv
  (glblRdrEnv, Just renamedSrc) <- runGhc (Just libdir) $ parseSourceFile LoadAllTargets (modPath sEnv) >>= rnWithGlobalEnv
  print . showPprUnsafe =<< searchOccName @IO sEnv glblRdrEnv
  putStrLn "built ..."
  print . showPprUnsafe =<< renamedSourceToBindings renamedSrc

printGatheredSEnv :: IO ()
printGatheredSEnv = commandLineInterface >>= print


commandLineInterface :: IO SearchEnv
commandLineInterface = do
  execParser (info (parseSearchEnv <**> helper)
                          (fullDesc <> progDesc "Print recursive declerations of an entity"
                           <> header "A different approach to showing outgoing call hierarchy for Haskell source code."))

getSearchEnv :: IO SearchEnv
getSearchEnv = commandLineInterface


-- printModuleDependency :: FilePath -> IO ()
-- printModuleDependency filePath = runGhc (Just libdir) $ do
--   let fileModuleName = mkFileModName filePath
--   env <- getSession
--   dflags <- getSessionDynFlags
--   setSessionDynFlags $ dflags { backend = NoBackend }
--   target <- guessTarget filePath Nothing
--   setTargets [target]
--   load LoadAllTargets
--   modGraph <- depanal [] True
--   liftIO $ print $ showPprUnsafe modGraph
