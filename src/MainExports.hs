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
import Symbols (symbolPG)
import Queries ()

import Database.PipesGremlin (PG,printPG,gather,scatter,nodesByLabel)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (StateT,runStateT)

import Data.Set (Set,empty)


gatherSymbols :: (Monad m) => PG (StateT (Set Integer) m) ()
gatherSymbols =
    nodesByLabel "Instance" >>=
    symbolPG

main :: IO ()
main = do
    runStateT (printPG gatherSymbols) empty >>= print
    return ()
