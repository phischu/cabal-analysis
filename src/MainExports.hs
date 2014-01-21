{-# LANGUAGE OverloadedStrings #-}
module Main where

import Exports (exportsPG)

import Database.PipesGremlin (PG,printPG,gather,scatter,nodesByLabel)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (StateT,runStateT)

import Data.Set (Set,empty)

import System.Process (system)
import Control.Concurrent (threadDelay)

import Control.Monad (void)


gatherSymbols :: (Monad m) => PG (StateT (Set Integer) m) ()
gatherSymbols =
    nodesByLabel "Instance" >>=
    exportsPG

main :: IO ()
main = do
    void (system "neo4j-community-2.0.0/bin/neo4j start")
    threadDelay 1000000
    runStateT (printPG gatherSymbols) empty >>= print
    return ()
