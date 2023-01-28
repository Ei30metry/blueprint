module Main (main) where

import           CLI                    ( parseSearchEnv )

import           Compute                ( occNameFromEntity, parseSourceFile,
                                          parsedToGlobalRdrEnv',
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
                                          desugarModule, getModSummary,
                                          getSessionDynFlags, guessTarget, load,
                                          mkModuleName, modInfoExports,
                                          modInfoTyThings, parseModule, runGhc,
                                          runGhcT, setSessionDynFlags,
                                          setTargets, typecheckModule )
import           GHC.Paths              ( libdir )
import           GHC.Types.Name.Reader  ( GlobalRdrElt, GlobalRdrEnv,
                                          lookupGlobalRdrEnv )
import           GHC.Utils.Outputable   ( showPprUnsafe )

import           Options.Applicative    ( execParser, fullDesc, header, helper,
                                          info, progDesc )

import           Types                  ( SearchEnv (..) )



-- TODO we should be able to compose and then runGhc at last
main :: IO ()
main = do
  sEnv <- getSearchEnv
  glblRdrEnv <- runGhc (Just libdir) $ parseSourceFile (modPath sEnv) >>= parsedToGlobalRdrEnv'
  print . showPprUnsafe =<< searchOccName @IO sEnv glblRdrEnv

printGatheredSEnv :: IO ()
printGatheredSEnv = commandLineInterface >>= print


commandLineInterface :: IO SearchEnv
commandLineInterface = do
  execParser (info (parseSearchEnv <**> helper)
                          (fullDesc <> progDesc "Print recursive declerations of an entity"
                           <> header "A different approach to showing outgoing call hierarchy for Haskell source code."))

getSearchEnv :: IO SearchEnv
getSearchEnv = commandLineInterface
