module Main where

import           App                        ( runBluePrint )

import           Blueprint                  ( initializeGhc, parseSourceFile',
                                              prototypeFunc, seeFromTcGblEnv )

import           CLI                        ( parseSearchEnv )

import           Compute                    ( entityToGlbRdrElt, entityToName,
                                              rnWithGlobalEnv',
                                              searchInDefUses )

import GHC.Types.Name.Occurrence (HasOccName(..), occNameString)
import           Control.Applicative        ( (<**>) )
import           Control.Monad              ( (<=<) )
import           Control.Monad.IO.Class     ( liftIO )

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Functor.Classes       ( eq1 )
import           Data.IORef                 ( readIORef )
import           Data.Text                  ( pack )
import           Data.Tree                  ( drawTree )

import           Diagrams.Backend.SVG       ( renderSVG, renderSVG' )

import           GHC                        ( GhcMonad (getSession),
                                              LoadHowMuch (LoadAllTargets),
                                              getModuleGraph, mgModSummaries,
                                              moduleUnit, parseModule, runGhcT )
import           GHC.Data.OrdList           ( fromOL )
import           GHC.Paths                  ( libdir )
import           GHC.Plugins                ( sizeUniqSet )
import           GHC.Tc.Types               ( TcGblEnv (..) )
import           GHC.Types.Name.Reader      ( GlobalRdrElt (gre_imp, gre_lcl),
                                              ImpDeclSpec (is_mod),
                                              ImportSpec (is_decl, is_item),
                                              pprGlobalRdrEnv )
import           GHC.Utils.Outputable       ( ppr, showPprUnsafe )

import           Options.Applicative        ( execParser, fullDesc, header,
                                              helper, info, progDesc )

import           Result                     ( bluePrintASTtoTreeString',
                                              defPrint, pprBAST,
                                              prettyPrintJSON,
                                              createImage', createImage)

import           Types                      ( SearchEnv (..) )


main :: IO ()
main = runner4


runner1 :: IO ()
runner1 = runGhcT (Just libdir) $ do
    sEnv <- liftIO getSearchEnv
    let ent = entity sEnv
    let filePath = modPath sEnv
    modSum <- initializeGhc filePath
    (result, _) <- runBluePrint (seeFromTcGblEnv @String tcg_dus) modSum
    (result2, _) <- runBluePrint (seeFromTcGblEnv @String tcg_used_gres) modSum
    fResult2 <- liftIO $ readIORef result2
    -- liftIO . defPrint . ppr $ result
    liftIO . defPrint . ppr $ toBinds $ fromOL result
  where
    toBinds = filter (\x -> fmap sizeUniqSet (fst x) `eq1` Just 1)
  -- liftIO . printSDocLn defaultSDocContext (PageMode True) stdout . ppr $ fResult2

runner2 :: IO ()
runner2 = runGhcT (Just libdir) $ do
  let path = "/Users/artin/Programming/projects/blueprint/test/golden/Golden2.hs"
  parsed <- parseSourceFile' LoadAllTargets path
  gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
  liftIO . defPrint $ pprGlobalRdrEnv True gblEnv


runner3 :: IO ()
runner3 = runGhcT (Just libdir) $ do
    sEnv <- liftIO getSearchEnv
    let ent = entity sEnv
    let filePath = modPath sEnv
    modSum <- initializeGhc filePath
    parsed <- parseModule modSum
    gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
    let glbRdrElt = entityToGlbRdrElt ent gblEnv
    let modNames = fmap (fmap (is_mod . is_decl) . gre_imp) glbRdrElt
    modSummaries <- mgModSummaries <$> getModuleGraph
    hsc <- getSession
    liftIO . defPrint $ ppr glbRdrElt


runner4 :: IO ()
runner4 = runGhcT (Just libdir) $ do
    sEnv <- liftIO getSearchEnv
    let ent = entity sEnv
    let filePath = modPath sEnv
    modSum <- initializeGhc filePath
    parsed <- parseModule modSum
    gblEnv <- return . fst <=< rnWithGlobalEnv' $ parsed
    (result, _) <- runBluePrint (seeFromTcGblEnv @String tcg_dus) modSum
    (res, _) <- runBluePrint (searchInDefUses @String result) (gblEnv, ent)
    let name = entityToName ent gblEnv
    -- mapM_ (liftIO . B.putStrLn . prettyPrintJSON) res
    case res of
      Just x  -> do
        liftIO . putStrLn . pprBAST $ x
        let tree = fmap (occNameString . occName) x
        liftIO $ createImage tree
      Nothing -> return ()


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
