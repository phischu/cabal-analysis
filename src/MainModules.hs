{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types (Module(Module))
import Database (resetDatabase)
import Types (Repository)
import Repository (loadRepository)
import Packages (packagePG)
import Versions (versionPG)
import Variants (variantPG)
import Targets (targetPG)
import Instances (instancePG)
import Modules (modulePG)

import Database.PipesGremlin (PG,printPG,gather,scatter,nodesByLabel)

import Control.Monad.IO.Class (MonadIO)
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

gatherInstances :: (Monad m) => PG (StateT (Set Integer) m) String
gatherInstances =
    nodesByLabel "Target" >>=
    instancePG >>=
    return . show

gatherModules :: (MonadIO m) => Repository -> PG m String
gatherModules repository = 
    nodesByLabel "Instance" >>=
    modulePG repository >>=
    return . show . (\(Module _ modulename _) -> modulename) . fst


main ::IO ()
main = do
    repository <- loadRepository
    resetDatabase
    printPG (gatherTargets repository)
    runStateT (printPG gatherInstances) empty >>= print
    printPG (gatherModules repository)

