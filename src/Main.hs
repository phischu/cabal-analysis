{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types (Repository)
import Repository (loadRepository)
import Packages (packagePG)
import Versions (versionPG)
import Variants (variantPG)
import Targets (targetPG)

import Database.PipesGremlin (PG,runPG,gather,scatter)
import Web.Neo (defaultRunNeoT)

import Control.Monad.IO.Class (MonadIO)

import Control.Proxy (runProxy,(>->),printD,hoist,lift,(>=>))

masterpipe :: (MonadIO m) => Repository -> PG m String
masterpipe repository =
    gather (packagePG repository) >>=
    scatter >>=
    versionPG repository >>=
    variantPG repository >>=
    targetPG  repository >>=
    return . show

main ::IO ()
main = do
    repository <- loadRepository
    defaultRunNeoT (runProxy ((const (runPG (masterpipe repository)) >-> (hoist (lift . lift) .) printD))) >>= print
