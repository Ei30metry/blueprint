module Main (main) where

import           CLI                    ( parseSearchEnv )

import           Compute                ( occNameFromEntity, parseSourceFile,
                                          prototypeFunc,
                                          renamedSourceToBindings,
                                          rnWithGlobalEnv, searchOccName )

import           Control.Applicative    ( (<**>) )
import           Control.Monad          ( (<=<), (>=>) )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Trans    ( lift )

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
main = printBindings "/Users/artin/Programming/projects/blueprint/test/golden/Golden5.hs"

prototype :: IO ()
prototype = runGhcT (Just libdir) $ do
  sEnv <- liftIO getSearchEnv
  -- (glblRdrEnv, _) <- parseSourceFile LoadAllTargets (modPath sEnv) >>= rnWithGlobalEnv
  myTuple <- parseSourceFile LoadAllTargets (modPath sEnv) >>= rnWithGlobalEnv
  renSrc <- return . (\(Just x) -> x) $ snd myTuple
  liftIO $ print . showPprUnsafe =<< searchOccName @IO sEnv (fst myTuple)
  bindings <- renamedSourceToBindings renSrc
  liftIO . print $ showPprUnsafe bindings


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
