{-# LANGUAGE OverloadedStrings #-}
module Instances where

import Types (
    PackageNode,
    TargetNode,
    InstanceNode)

import Web.Neo (NeoT,newNode,addNodeLabel,newEdge)
import Web.Neo.Internal (nodeId)
import Database.PipesGremlin (PG,gather,has,strain,nodeProperty,nextLabeled)

import Control.Monad (forM_,(>=>))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT,get,put)

import Data.Set (Set,member,insert)

instancePG :: (Monad m) => TargetNode -> StateT (Set Integer) (PG m) InstanceNode
instancePG targetnode = do

    visitedTargetNodeIds <- get
    put (insert (nodeId targetnode) visitedTargetNodeIds)

    if nodeId targetnode `member` visitedTargetNodeIds

        then lift (return targetnode >>= nextLabeled "INSTANCE")

        else do

            packagedependencynodes <- lift (gather (dependencies targetnode))
            instancedependencies <- mapM
                (lift . libraryTargets >=> instancePG)
                packagedependencynodes

            instancenode <- lift (lift (insertInstance instancedependencies targetnode))

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
