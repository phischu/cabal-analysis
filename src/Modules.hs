{-# LANGUAGE OverloadedStrings #-}
module Modules where

import Modules.JSON ()
import Modules.RecoverInstance (recoverInstance)

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

insertModule :: (Monad m) => ModuleName -> ModuleAST -> InstanceNode -> NeoT m ModuleNode
insertModule modulename moduleast instancenode = do
    modulenode <- newNode
    addNodeLabel "Module" modulenode
    setNodeProperty "modulename" (toJSON modulename) modulenode
    setNodeProperty "moduleast" (toJSON moduleast) modulenode
    _ <- newEdge "MODULE" instancenode modulenode
    return modulenode
