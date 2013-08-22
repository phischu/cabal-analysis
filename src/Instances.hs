{-# LANGUAGE OverloadedStrings #-}
module Instances where

import Types (
    PackageNode,
    TargetNode,
    InstanceNode)

import Web.Neo (NeoT,newNode,addNodeLabel,newEdge)
import Database.PipesGremlin (PG,scatter,gather,has,strain,nodeProperty,nextLabeled)

import Control.Monad (forM_,(>=>))
import Control.Monad.Trans (lift)

instancePG :: (Monad m) => TargetNode -> PG m InstanceNode
instancePG targetnode = do

    existingInstances <- gather (return targetnode >>= nextLabeled "INSTANCE")

    if not (null existingInstances)
        then scatter existingInstances
        else do

            packagedependencynodes <- gather (dependencies targetnode)
            instancedependencies <- mapM (libraryTargets >=> instancePG) packagedependencynodes

            instancenode <- lift (insertInstance instancedependencies targetnode)

            return instancenode

dependencies :: (Monad m) => TargetNode -> PG m PackageNode
dependencies = nextLabeled "PACKAGEDEPENDENCY"

libraryTargets :: (Monad m) => PackageNode -> PG m TargetNode
libraryTargets =
    nextLabeled "VERSION" >=>
    nextLabeled "VARIANT" >=>
    nextLabeled "TARGET" >=>
    has (nodeProperty "targettype" >=> strain (== ("LibraryTarget" :: String)))

insertInstance :: (Monad m) => [InstanceNode] -> TargetNode -> NeoT m InstanceNode
insertInstance instancedependencies targetnode = do
    instancenode <- newNode
    addNodeLabel "Instance" instancenode
    forM_ instancedependencies (newEdge "INSTANCEDEPENDENCY" instancenode)
    _ <- newEdge "INSTANCE" targetnode instancenode
    return instancenode
