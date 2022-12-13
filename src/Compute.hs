{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}


module Compute where

import           Control.Monad.Trans.Class  ( lift )

import           Data.Foldable              ( find )

import           GHC                        ( Ghc, GhcMonad (..),
                                              LoadHowMuch (..),
                                              ParsedModule (..), addTarget,
                                              depanal, guessTarget, load,
                                              mgModSummaries, parseModule )
import           GHC.Runtime.Loader
import           GHC.Types.Target           ( Target (..) )
import           GHC.Unit.Module.ModSummary ( msHsFilePath )
import           GHC.Utils.Panic            ( panic )


toAST :: GhcMonad m => FilePath -> m ParsedModule
toAST fn = do
  target <- guessTarget fn Nothing
  addTarget target
  _ <- load LoadAllTargets
  modGraph <- depanal [] True
  case (find ((== fn) . msHsFilePath) (mgModSummaries modGraph)) of
    Just modSummary -> parseModule modSummary
    Nothing -> panic "target FilePath not found in module dependency graph"
