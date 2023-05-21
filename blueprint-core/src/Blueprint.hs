{-
This module is the frontend to all the functionalities provided by blueprint and
should be the only module needed in order to make a GUI client for Blueprint
-}

module Blueprint where

import           App                           ( BluePrint (..) )

import           Compute.AST                   ( parseSourceFile,
                                                 rnWithGlobalEnv',
                                                 tcModuleToTcGblEnv,
                                                 valBindsToHsBinds )
import           Compute.Morphisms             ( entityToGlbRdrElt )

import           Control.Monad.Reader
import           Control.Monad.Trans.Except    ( ExceptT, except, throwE )

import           GHC                           ( Backend (NoBackend), GhcRn,
                                                 HsGroup (hs_valds),
                                                 LoadHowMuch (..),
                                                 ModSummary (..), Name (..),
                                                 ParsedModule (..),
                                                 RenamedSource, backend,
                                                 getModSummary,
                                                 getSessionDynFlags,
                                                 guessTarget, hs_valds, load,
                                                 mkModuleName, parseModule,
                                                 setSessionDynFlags, setTargets,
                                                 typecheckModule )
import           GHC.Driver.Monad              ( GhcMonad (..) )
import           GHC.Driver.Session
import           GHC.Tc.Types                  ( TcGblEnv (..) )
import           GHC.Types.Name.Reader         ( GlobalRdrElt )
import           GHC.Types.Name.Set            ( DefUses )

import           HIE.Bios                      hiding ( initSession )
import           HIE.Bios.Environment

import           Language.Haskell.Syntax.Binds ( HsValBinds )

import           System.Directory

import           Types                         ( Entity )



-- FIXME terrible performance
-- findModuleName :: String -> Either String String
findModuleName :: Monad m => String -> ExceptT String m String
findModuleName = nonMainModule . dropWhile (/= "module") . words
  where nonMainModule []      = throwE "Empty file"
        nonMainModule (_:x:_) = return x
        nonMainModule _       = throwE "Couldn't file module name"


-- | Sets up the right environment for ghc to compute on
initializeGhc :: (GhcMonad m) => FilePath -> ExceptT String m ModSummary
initializeGhc filePath = do
    fileContent <- liftIO $ readFile filePath
    fileModuleName <- findModuleName fileContent
    dir <- liftIO getCurrentDirectory
    maybeCradle <- liftIO $ findCradle (dir <> "/")
    mPath <- convertCradle maybeCradle
    p <- liftIO $ return mPath
    cradle <- liftIO (loadCradle p)
    comp <- liftIO $ getCompilerOptions filePath cradle
    setupOptions (fileModuleName, comp)

  where
    convertCradle (Just x) = return x
    convertCradle Nothing  = throwE "Coudln't locate cradle"

    setupOptions (modName , CradleSuccess r) = do
            dflags <- lift $ initSession r >> getSessionDynFlags
            lift . setSessionDynFlags $ dflags { backend = NoBackend, ghcLink = LinkInMemory, ghcMode = CompManager }
            target <- lift $ guessTarget filePath Nothing
            lift $ setTargets [target] >> load LoadAllTargets
            lift $ getModSummary $ mkModuleName modName

    setupOptions _ = throwE "Cradle failed"


rnSrcToBinds' :: GhcMonad m => RenamedSource -> m (HsValBinds GhcRn)
rnSrcToBinds' = return . hs_valds . \(b,_,_,_) -> b


parseSourceFile' :: GhcMonad m => LoadHowMuch -> FilePath -> m ParsedModule
parseSourceFile' loadHowMuch filePath = do
    let fileModuleName = mkFileModName filePath
    dflags <- getSession >> getSessionDynFlags
    setSessionDynFlags $ dflags { backend = NoBackend }
    target <- guessTarget filePath Nothing
    setTargets [target]
    load loadHowMuch -- TODO construct a Unit in order to feed your path to your module
    modSum <- getModSummary $ mkModuleName fileModuleName
    parseModule modSum
  where
    mkFileModName = reverse . takeWhile (/= '/') . reverse . (\fp -> take (length fp - 3) fp)


prototypeFunc :: GhcMonad m => FilePath -> m [Name]
prototypeFunc = return . valBindsToHsBinds <=< rnSrcToBinds' <=< return . snd
                <=< rnWithGlobalEnv' <=< parseSourceFile' LoadAllTargets


rnTest ::  forall w m. (GhcMonad m, Monoid w) => Entity -> BluePrint String ModSummary w m GlobalRdrElt
rnTest ent = BT $ do
  parsed <- unBluePrint parseSourceFile
  (glb,_) <- lift . lift . lift $ rnWithGlobalEnv' parsed
  lift . except $ entityToGlbRdrElt ent glb


seeFromTcGblEnv :: forall w s m e. (GhcMonad m, Monoid w) => (TcGblEnv -> s) -> BluePrint e ModSummary w m s
seeFromTcGblEnv fieldSelector = BT $ do
  parsed <- unBluePrint parseSourceFile
  lift . lift . lift $ return . fieldSelector . tcModuleToTcGblEnv <=< typecheckModule $ parsed


seeDefUses :: forall w m e. (GhcMonad m, Monoid w) => BluePrint e ModSummary w m DefUses
seeDefUses = seeFromTcGblEnv tcg_dus
