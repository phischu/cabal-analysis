module Database where

import System.Process (system)
import System.Directory (removeDirectoryRecursive,createDirectory)
import Control.Concurrent (threadDelay)

resetDatabase :: IO ()
resetDatabase = do
    system "neo4j-community-2.0.0-M04/bin/neo4j stop"
    removeDirectoryRecursive "neo4j-community-2.0.0-M04/data/"
    createDirectory "neo4j-community-2.0.0-M04/data"
    system "neo4j-community-2.0.0-M04/bin/neo4j start"
    threadDelay 1000000
