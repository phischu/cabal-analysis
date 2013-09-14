{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Modules where

import Modules.JSON ()

import Types (
    Repository,
    Package(Package),
    Version(Version),
    Variant(Variant),
    Target(Target),
    InstanceNode,Instance(Instance),
    ModuleNode,ModuleName,ModuleAST)

import Database.PipesGremlin (
    PG,previousLabeled,followingLabeled,nodeProperty,gather)
import Web.Neo (
    NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)

import Data.Aeson (toJSON)

import Control.Monad.Trans (lift)

modulePG :: (Monad m) => Repository -> InstanceNode -> PG m (ModuleName,ModuleNode)
modulePG repository instancenode = do
    inst <- recoverInstance instancenode
    let modulename = undefined
        moduleast  = undefined
    modulenode <- lift (insertModule modulename moduleast instancenode)
    return (modulename,modulenode)

recoverInstance :: (Monad m) => InstanceNode -> PG m Instance
recoverInstance instancenode = do
    targetnode  <- previousLabeled "INSTANCE" instancenode
    variantnode <- previousLabeled "TARGET" targetnode
    versionnode <- previousLabeled "VARIANT" variantnode
    packagenode <- previousLabeled "VERSION" versionnode

    packagename <- nodeProperty "packagename" packagenode
    versionnumber <- nodeProperty "versionnumber" versionnode >>= return . read
    configuration <- nodeProperty "configuration" variantnode >>= return . read
    targettype <- nodeProperty "targettype" targetnode >>= return . read
    packagedependencies <- gather (
        return targetnode >>=
        followingLabeled "PACKAGEDEPENDENCY" >>=
        nodeProperty "packagename")
    instancedependencies <- gather (
        return instancenode >>=
        followingLabeled "INSTANCEDEPENDENCY")

    return (Instance (Target (Variant (Version (Package
        packagename)
        versionnumber)
        configuration)
        targettype
        packagedependencies)
        instancedependencies)

insertModule :: (Monad m) => ModuleName -> ModuleAST -> InstanceNode -> NeoT m ModuleNode
insertModule modulename moduleast instancenode = do
    modulenode <- newNode
    addNodeLabel "Module" modulenode
    setNodeProperty "modulename" (toJSON modulename) modulenode
    setNodeProperty "moduleast" (toJSON moduleast) modulenode
    _ <- newEdge "MODULE" instancenode modulenode
    return modulenode

