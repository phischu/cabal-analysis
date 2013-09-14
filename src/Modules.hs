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

import Text.Read (readEither)

import Control.Error (runEitherT,EitherT,left)

import Control.Monad (mzero)
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
    instancesOrErrors <- gather (runEitherT (tryRecoverInstance instancenode))
    let instanceOrError = case instancesOrErrors of
            []        -> Left NoInstanceRecovered
            [Left e]  -> Left e
            [Right i] -> Right i
            _         -> Left MultipleInstancesRecovered
    case instanceOrError of
        Left e  -> lift (insertInstanceRecoverError e instancenode) >> mzero
        Right i -> return i

tryRecoverInstance :: (Monad m) => InstanceNode -> EitherT InstanceRecoverError (PG m) Instance
tryRecoverInstance instancenode = do
    targetnode  <- lift (previousLabeled "INSTANCE" instancenode)
    variantnode <- lift (previousLabeled "TARGET" targetnode)
    versionnode <- lift (previousLabeled "VARIANT" variantnode)
    packagenode <- lift (previousLabeled "VERSION" versionnode)

    packagename <- lift (nodeProperty "packagename" packagenode)
    versionnumber <- lift (nodeProperty "versionnumber" versionnode) >>= either (left . VersionNumberParseError) return . readEither
    configuration <- lift (nodeProperty "configuration" variantnode) >>= either (left . ConfigurationParseError) return . readEither
    targettype <- lift (nodeProperty "targettype" targetnode) >>= either (left . TargetTypeParseError) return . readEither
    packagedependencies <- lift (gather (
        return targetnode >>=
        followingLabeled "PACKAGEDEPENDENCY" >>=
        nodeProperty "packagename"))
    instancedependencies <- lift (gather (
        return instancenode >>=
        followingLabeled "INSTANCEDEPENDENCY"))

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

data InstanceRecoverError =
    VersionNumberParseError String |
    ConfigurationParseError String |
    TargetTypeParseError String |
    NoInstanceRecovered |
    MultipleInstancesRecovered
      deriving (Show,Read)

insertInstanceRecoverError :: (Monad m) => InstanceRecoverError -> InstanceNode -> NeoT m ()
insertInstanceRecoverError instancerecovererror instancenode = do
    instancerecovererrornode <- newNode
    addNodeLabel "Error" instancerecovererrornode
    addNodeLabel "InstanceRecoverError" instancerecovererrornode
    _ <- newEdge "ERROR" instancenode instancerecovererrornode
    return ()

