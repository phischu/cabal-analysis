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

import Database.PipesGremlin (PG,runPG,gather,scatter,nodesByLabel)
import Web.Neo (defaultRunNeoT)

import Control.Monad.IO.Class (MonadIO)

import Control.Proxy (runProxy,(>->),printD,hoist,lift)

masterpipe :: (MonadIO m) => Repository -> PG m String
masterpipe repository = gather (
    gather (packagePG repository) >>=
    scatter >>=
    versionPG repository >>=
    variantPG repository >>=
    targetPG  repository) >>
    nodesByLabel "Target" >>=
    instancePG >>=
    return . show

main ::IO ()
main = do
    repository <- loadRepository
    resetDatabase
    defaultRunNeoT (runProxy ((const (runPG (masterpipe repository)) >-> (hoist (lift . lift) .) printD))) >>= print
