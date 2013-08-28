{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database (resetDatabase)
import Types (Repository)
import Repository (loadRepository)
import Packages (packagePG)
import Versions (versionPG)
import Variants (variantPG)
import Targets (targetPG)
import Instances (instancePG)
import Queries ()

import Database.PipesGremlin (PG,printPG,gather,scatter,nodesByLabel)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT,runStateT)

import Data.Set (Set,empty)

gatherTargets :: (MonadIO m) => Repository -> PG m [String]
gatherTargets repository = gather (
    gather (packagePG repository) >>=
    scatter >>=
    versionPG repository >>=
    variantPG repository >>=
    targetPG  repository >>=
    return . show)

gatherInstances :: (Monad m) => StateT (Set Integer) (PG m) String
gatherInstances =
    lift (nodesByLabel "Target") >>=
    instancePG >>=
    return . show

main ::IO ()
main = do
    repository <- loadRepository
    resetDatabase
    printPG (gatherTargets repository)
    printPG (runStateT gatherInstances empty)
