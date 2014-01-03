module Database where

import System.Process (system)
import System.Directory (removeDirectoryRecursive,createDirectory)
import Control.Concurrent (threadDelay)

import Control.Monad (void)

resetDatabase :: IO ()
resetDatabase = do
    void (system "neo4j-community-2.0.0/bin/neo4j stop")
    removeDirectoryRecursive "neo4j-community-2.0.0/data/"
    createDirectory "neo4j-community-2.0.0/data"
    void (system "neo4j-community-2.0.0/bin/neo4j start")
    threadDelay 1000000
