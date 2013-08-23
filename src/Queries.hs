{-# LANGUAGE OverloadedStrings #-}
module Queries where

import Database.PipesGremlin (
    printPG,PG,
    gather,hasnot,
    nodesByLabel,outEdgeLabeled,nodeProperty,
    previousLabeled,nextLabeled,has,strain)
import Web.Neo (Node)

import Control.Monad ((>=>))

gatherPrintPG :: (Show a) => PG IO a -> IO ()
gatherPrintPG = printPG . gather

packageWithName :: String -> PG IO Node
packageWithName packagename =
    nodesByLabel "Package" >>=
    has (nodeProperty "packagename" >=> strain (== packagename))

reverseDependencies :: String -> PG IO (String,String)
reverseDependencies packagename =
    packageWithName packagename >>=
    previousLabeled "PACKAGEDEPENDENCY" >>=
    previousLabeled "TARGET" >>=
    previousLabeled "VARIANT" >>=
    (\version -> do
        versionnumber <- nodeProperty "versionnumber" version
        reverseDependencyName <- return version >>= previousLabeled "VERSION" >>= nodeProperty "packagename"
        return (reverseDependencyName,versionnumber))

packagesWithNoVersion :: PG IO String
packagesWithNoVersion = 
    nodesByLabel "Package" >>=
    hasnot (outEdgeLabeled "VERSION") >>=
    nodeProperty "packagename"

packagesWithNoVersionResult :: [String]
packagesWithNoVersionResult = [
    "StateVar","Tensor","ObjectName","ghc-prim","extensible-exceptions",
    "ghc","bytestring-lexing","integer","rts","integer-simple",
    "base-unicode-symbols","MonadCatchIO-mtl","ghc-paths","integer-gmp",
    "packedstring","primitive","fps"]


