{-# LANGUAGE OverloadedStrings #-}
module Modules where

import Modules.JSON ()
import Modules.RecoverInstance (recoverInstance)
import Variants (loadPackageDescription)
import Targets (finalize)

import Types (
    Repository,
    Package(Package),
    Version(Version),
    Variant(Variant),
    Target(Target),
    InstanceNode,Instance(Instance),
    ModuleNode,ModuleName,ModuleAST,
    FinalizedPackageDescription)

import Database.PipesGremlin (
    PG,previousLabeled,followingLabeled,nodeProperty,gather)
import Web.Neo (
    NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)

import Data.Aeson (toJSON)

import Text.Read (readEither)

import Control.Error (runEitherT,EitherT,left)

import Control.Monad (mzero)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)

modulePG :: (MonadIO m) => Repository -> InstanceNode -> PG m (ModuleName,ModuleNode)
modulePG repository instancenode = do
    inst <- recoverInstance instancenode
    let (Instance (Target variant _ _) _) = inst
    packageDescription <- getFinalizedPackageDescription repository variant instancenode

    let modulename = undefined
        moduleast  = undefined
    modulenode <- lift (insertModule modulename moduleast instancenode)
    return (modulename,modulenode)

insertModule :: (Monad m) => ModuleName -> ModuleAST -> InstanceNode -> NeoT m ModuleNode
insertModule modulename moduleast instancenode = do
    modulenode <- newNode
    addNodeLabel "Module" modulenode
    setNodeProperty "modulename" (toJSON modulename) modulenode
    setNodeProperty "moduleast" (toJSON moduleast) modulenode
    _ <- newEdge "MODULE" instancenode modulenode
    return modulenode

getFinalizedPackageDescription :: (MonadIO m) =>
    Repository ->
    Variant ->
    InstanceNode ->
    PG m FinalizedPackageDescription
getFinalizedPackageDescription repository variant instancenode =
    tryGetFinalizedPackageDescription repository variant >>=
    maybe (lift (insertGetFinalizedPackageDescriptionError instancenode) >> mzero) return

tryGetFinalizedPackageDescription :: (MonadIO m) =>
    Repository ->
    Variant ->
    m (Maybe FinalizedPackageDescription)
tryGetFinalizedPackageDescription repository variant = do
    let (Variant version configuration) = variant
    packageDescription <- loadPackageDescription repository version
    return (finalize configuration packageDescription)

insertGetFinalizedPackageDescriptionError ::(Monad m) => InstanceNode -> NeoT m ()
insertGetFinalizedPackageDescriptionError instancenode = do
    getfinalizedpackagedescriptionerrornode <- newNode
    addNodeLabel "Error" getfinalizedpackagedescriptionerrornode
    addNodeLabel "GetFinalizedPackageDescriptionError" getfinalizedpackagedescriptionerrornode
    setNodeProperty
        "error"
        (toJSON ("Unable to getFinalizedPackageDescription" :: String))
        getfinalizedpackagedescriptionerrornode
    _ <- newEdge "ERROR" instancenode getfinalizedpackagedescriptionerrornode
    return ()


