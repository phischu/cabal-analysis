{-# LANGUAGE OverloadedStrings #-}
module Instances where

import Types (
    PackageNode,
    TargetNode,
    InstanceNode)

import Web.Neo (NeoT,newNode,addNodeLabel,newEdge)
import Web.Neo.Internal (nodeId)
import Database.PipesGremlin (PG,gather,has,strain,nodeProperty,followingLabeled,previousLabeled)
import Pipes (ListT(Select,enumerate),(>->))
import qualified Pipes.Prelude as Pipes (take)

import Control.Monad (forM_,(>=>),guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT,get,put)

import Data.Set (Set,member,insert)
import Data.List (nub)

instancePG :: (Monad m) => TargetNode ->  PG (StateT (Set Integer) m) InstanceNode
instancePG targetnode = limit 1 (do

    visitedTargetNodeIds <- lift ( lift (lift get))
    lift (lift (lift (put (insert (nodeId targetnode) visitedTargetNodeIds))))

    if nodeId targetnode `member` visitedTargetNodeIds

        then return targetnode >>= followingLabeled "INSTANCE"

        else do

            packagedependencynodes <- gather (dependencies targetnode)
            instancedependencies <- mapM
                (libraryTargets >=> instancePG)
                packagedependencynodes

            containsNoDuplicatePackages instancedependencies targetnode >>= guard

            instancenode <- lift (insertInstance instancedependencies targetnode)

            return instancenode)

dependencies :: (Monad m) => TargetNode -> PG m PackageNode
dependencies = followingLabeled "PACKAGEDEPENDENCY"

libraryTargets :: (Monad m) => PackageNode -> PG m TargetNode
libraryTargets =
    followingLabeled "VERSION" >=>
    followingLabeled "VARIANT" >=>
    followingLabeled "TARGET" >=>
    has (nodeProperty "targettype" >=> strain (== ("LibraryTarget" :: String)))

containsNoDuplicatePackages :: (Monad m) => [InstanceNode] -> TargetNode -> PG m Bool
containsNoDuplicatePackages instancedependencies targetnode = do
    targetnodes <- mapM (previousLabeled "INSTANCE") instancedependencies
    let uniqueTargets = nub (targetnode:targetnodes)
    packagenames <- mapM targetPackageName uniqueTargets
    return (nub packagenames == packagenames)

targetPackageName :: (Monad m) => TargetNode -> PG m String
targetPackageName =
    previousLabeled "TARGET" >=>
    previousLabeled "VARIANT" >=>
    previousLabeled "VERSION" >=>
    nodeProperty "packagename"

insertInstance :: (Monad m) => [InstanceNode] -> TargetNode -> NeoT m InstanceNode
insertInstance instancedependencies targetnode = do
    instancenode <- newNode
    addNodeLabel "Instance" instancenode
    forM_ instancedependencies (newEdge "INSTANCEDEPENDENCY" instancenode)
    _ <- newEdge "INSTANCE" targetnode instancenode
    return instancenode

limit :: (Monad m) => Int -> PG  m a -> PG m a
limit n pg = Select (enumerate pg >-> Pipes.take n)
