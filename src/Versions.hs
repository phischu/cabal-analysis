{-# LANGUAGE OverloadedStrings #-}
module Versions where

import Types (Repository,Package(Package),PackageNode,Version(Version),VersionNumber,VersionNode)

import Web.Neo (NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)
import Database.PipesGremlin (PG,scatter)

import Data.Aeson (toJSON)

import Control.Monad.Trans (lift)

import Data.Map (keys,(!))

versionPG :: (Monad m) => Repository -> (Package,PackageNode) -> PG m (Version,VersionNode)
versionPG repository (package,packagenode) = do

    version@(Version _ versionnumber) <- scatter (versions repository package)

    versionnode <- lift (insertVersion versionnumber packagenode)

    return (version,versionnode)

versions :: Repository -> Package -> [Version]
versions repository package@(Package packagename) =
    map (Version package) (keys (repository ! packagename))

insertVersion :: (Monad m) => VersionNumber -> PackageNode -> NeoT m VersionNode
insertVersion versionnumber packagenode = do
    versionnode <- newNode
    addNodeLabel "Version" versionnode
    setNodeProperty "versionnumber" (toJSON (show versionnumber)) versionnode
    _ <- newEdge "VERSION" packagenode versionnode
    return versionnode
