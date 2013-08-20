{-# LANGUAGE OverloadedStrings #-}
module Variants where

import Types (
    Repository,
    Package(Package),
    Version(Version),VersionNumber,VersionNode,
    Variant(Variant),PackageDescription,Configuration,PackageDependency,VariantNode)

import Web.Neo (NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)
import Database.PipesGremlin (PG,scatter)

import Data.Aeson (toJSON)

import Data.Version (showVersion)

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)

import Data.Map (keys,(!))

variantPG :: (MonadIO m) => Repository -> (Version,VersionNode) -> PG m (Variant,VariantNode)
variantPG repository (version,versionnode) = do

    variant@(Variant _ configuration dependencies) <- variants repository version >>= scatter

    variantnode <- lift (insertVariant configuration dependencies versionnode)

    return (variant,variantnode)

variants :: (MonadIO m) => Repository -> Version -> m [Variant]
variants repository version = do
    description <- packageDescription repository version
    forM (configurations description) (\configuration ->
        return (Variant version configuration (dependencies configuration description)))

packageDescription :: Repository -> Version -> m PackageDescription
packageDescription = undefined

dependencies :: Configuration -> PackageDescription -> [PackageDependency]
dependencies = undefined

configurations :: PackageDescription -> [Configuration]
configurations = undefined

insertVariant :: (Monad m) => Configuration -> [PackageDependency] -> VersionNode -> NeoT m VariantNode
insertVariant versionnumber packagenode = undefined {-do
    versionnode <- newNode
    addNodeLabel "Version" versionnode
    setNodeProperty "versionnumber" (toJSON (showVersion versionnumber)) versionnode
    newEdge "VERSION" packagenode versionnode
    return versionnode-}


