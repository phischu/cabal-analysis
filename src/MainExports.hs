{-# LANGUAGE OverloadedStrings #-}
module Main where

import Exports (exportsPG)

import Database.PipesGremlin (PG,printPG,gather,scatter,nodesByLabel)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (StateT,runStateT)

import Data.Set (Set,empty)


gatherSymbols :: (Monad m) => PG (StateT (Set Integer) m) ()
gatherSymbols =
    nodesByLabel "Instance" >>=
    exportsPG

main :: IO ()
main = do
    runStateT (printPG gatherSymbols) empty >>= print
    return ()
