{-# LANGUAGE OverloadedStrings #-}
module Packages where

import Types (Repository,Package(Package),PackageName,PackageNode)

import Web.Neo (NeoT,newNode,addNodeLabel,setNodeProperty)
import Database.PipesGremlin (PG,scatter)

import Data.Aeson (toJSON)

import Control.Monad.Trans (lift)

import Data.Map (keys)

packagePG :: (Monad m) => Repository -> PG m (Package,PackageNode)
packagePG repository = do

    package@(Package packagename) <- scatter (packages repository)

    packagenode <- lift (insertPackage packagename)
    lift (setNodeProperty "packagename" (toJSON packagename) packagenode)

    return (package,packagenode)

packages :: Repository -> [Package]
packages = map Package . keys

insertPackage :: (Monad m) => PackageName -> NeoT m PackageNode
insertPackage packagename = do
    packagenode <- newNode
    addNodeLabel "Package" packagenode
    setNodeProperty "packagename" (toJSON packagename) packagenode
    return packagenode

