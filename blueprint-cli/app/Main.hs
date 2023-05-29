module Main where

import           Control.Applicative             ( (<**>) )
import           Control.Monad                   ( (<=<) )
import           Control.Monad.Except            ( ExceptT (ExceptT), runExcept,
                                                   runExceptT )
import           Control.Monad.IO.Class          ( liftIO )
import           Control.Monad.Trans
import           Control.Monad.Trans.Except      ( except )

import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy.Char8      as B
import           Data.Functor.Classes            ( eq1 )
import           Data.IORef                      ( readIORef )
import           Data.Text                       ( pack )
import           Data.Tree                       ( drawTree )

import           Development.Blueprint           ( initializeGhc, seeDefUses,
                                                   seeFromTcGblEnv )
import           Development.Blueprint.App       ( runBluePrint )
import           Development.Blueprint.Compute   ( entityToGlbRdrElt,
                                                   entityToName,
                                                   searchInDefUses, valBindsToHsBinds )
import           Development.Blueprint.Types     ( Entity (..), SearchEnv (..) )
import           Development.Blueprint.Types.AST ( BluePrintAST (..) )

import           GHC
import           GHC.Data.OrdList                ( fromOL )
import           GHC.Paths                       ( libdir )
import           GHC.Plugins                     ( GlobalRdrEnv, showPpr,
                                                   sizeUniqSet )
import           GHC.Tc.Types                    ( TcGblEnv (..) )
import           GHC.Types.Name                  ( Name (..) )
import           GHC.Types.Name.Occurrence       ( HasOccName (..),
                                                   occNameString )
import           GHC.Types.Name.Reader           ( GlobalRdrElt (gre_imp, gre_lcl),
                                                   ImpDeclSpec (is_mod),
                                                   ImportSpec (is_decl, is_item),
                                                   pprGlobalRdrEnv )
import           GHC.Types.Name.Set
import           GHC.Utils.Outputable            ( ppr, showPprUnsafe )


main :: IO ()
main = undefined -- runner4


-- runner1 :: IO ()
-- runner1 = runGhcT (Just libdir) $ do
--     sEnv <- liftIO getSearchEnv
--     let ent = entity sEnv
--     let filePath = modPath sEnv
--     modSum' <- runExceptT $ initializeGhc filePath
--     case modSum' of
--       Right modSum -> do
--             (result, _) <- runBluePrint (seeFromTcGblEnv @String tcg_dus) modSum
--             (result2, _) <- runBluePrint (seeFromTcGblEnv @String tcg_used_gres) modSum
--             fResult2 <- liftIO $ readIORef result2
--             -- liftIO . defPrint . ppr $ result
--             liftIO . defPrint . ppr $ toBinds $ fromOL result
--           where
--             toBinds = filter (\x -> fmap sizeUniqSet (fst x) `eq1` Just 1)
--           -- liftIO . printSDocLn defaultSDocContext (PageMode True) stdout . ppr $ fResult2
--       Left err -> liftIO $ putStrLn err


-- runner2 :: IO ()
-- runner2 = runGhcT (Just libdir) $ do
--   let path = "/Users/artin/Programming/projects/blueprint/test/golden/Golden2.hs"
--   parsed <- parseSourceFile' LoadAllTargets path
--   gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
--   liftIO . defPrint $ pprGlobalRdrEnv True gblEnv


-- runner3 :: IO ()
-- runner3 = runGhcT (Just libdir) $ do
--     sEnv <- liftIO getSearchEnv
--     let ent = entity sEnv
--     let filePath = modPath sEnv
--     modSum' <- runExceptT $ initializeGhc filePath
--     case modSum' of
--       Right modSum -> do
--           parsed <- parseModule modSum
--           gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
--           let glbRdrElt = entityToGlbRdrElt ent gblEnv
--           let modNames = fmap (fmap (is_mod . is_decl) . gre_imp) glbRdrElt
--           modSummaries <- mgModSummaries <$> getModuleGraph
--           hsc <- getSession
--           liftIO . defPrint $ ppr glbRdrElt
--       Left err-> liftIO $ putStrLn err


-- runner4 :: IO ()
-- runner4 = runGhcT (Just libdir) $ do
--     sEnv <- liftIO getSearchEnv
--     let ent = entity sEnv
--     let filePath = modPath sEnv
--     modSum' <- runExceptT $ initializeGhc filePath
--     case modSum' of
--       Right modSum -> do
--               parsed <- parseModule modSum
--               gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
--               (result, _) <- runBluePrint (seeDefUses @String) modSum
--               case result of
--                 Right x -> do
--                     (res, _) <- runBluePrint (searchInDefUses @String x) (gblEnv, ent)
--                     let name = entityToName ent gblEnv
--                     case res of
--                       Right result' -> do
--                         liftIO . putStrLn . pprBAST $ result'
--                       Left _ -> return ()
--                 Left err -> liftIO $ putStrLn err
--       Left x -> liftIO $ putStrLn x



-- runnerEither :: IO ()
-- runnerEither = runGhcT (Just libdir) $ do
--     sEnv <- liftIO getSearchEnv
--     let ent = entity sEnv
--     let filepath = modPath sEnv
--     x <- runExceptT $ t ent filepath
--     return ()
--   where
--     t :: forall m . GhcMonad m => Entity -> FilePath -> ExceptT String m (BluePrintAST Name)
--     t ent filePath = do
--         modSum <- initializeGhc filePath
--         parsed <- lift $ parseModule modSum
--         (gblEnv, _) <- lift $ rnWithGlobalEnv' parsed
--         let (result, _) = runBluePrint (seeFromTcGblEnv @String tcg_dus) modSum
--         test <- except result
--         (res, _) <- lift $ runBluePrint (searchInDefUses @String test) (gblEnv, ent)
--         test2 <- except res
--         name <- except $ entityToName ent gblEnv
--         return test2



-- printBindings :: FilePath -> IO ()
-- printBindings filePath = runGhcT (Just libdir) $ do
--   result <- prototypeFunc filePath
--   liftIO . print $ showPprUnsafe result


-- printGatheredSEnv :: IO ()
-- printGatheredSEnv = commandLineInterface >>= print


-- commandLineInterface :: IO SearchEnv
-- commandLineInterface = do
--   execParser (info (parseSearchEnv <**> helper)
--                           (fullDesc <> progDesc "Print recursive declerations of an entity"
--                            <> header "A different approach to showing outgoing call hierarchy for Haskell source code."))

-- getSearchEnv :: IO SearchEnv
-- getSearchEnv = commandLineInterface


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
