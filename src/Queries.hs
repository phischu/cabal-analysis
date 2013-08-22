{-# LANGUAGE OverloadedStrings #-}
module Queries where

import Database.PipesGremlin (
    printPG,PG,
    gather,hasnot,
    nodesByLabel,outEdgeLabeled,nodeProperty)

packagesWithNoVersion :: IO ()
packagesWithNoVersion = printPG (gather (
    nodesByLabel "Package" >>=
    hasnot (outEdgeLabeled "VERSION") >>=
    nodeProperty "packagename") :: PG IO [String])

packagesWithNoVersionResult :: [String]
packagesWithNoVersionResult = [
    "StateVar","Tensor","ObjectName","ghc-prim","extensible-exceptions",
    "ghc","bytestring-lexing","integer","rts","integer-simple",
    "base-unicode-symbols","MonadCatchIO-mtl","ghc-paths","integer-gmp",
    "packedstring","primitive","fps"]


