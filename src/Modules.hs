{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Modules where

import Modules.JSON ()

import Types (
    Repository,ModuleName,ModuleAST,InstanceNode,ModuleNode)

import Database.PipesGremlin (PG)
import Web.Neo (
    NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)

import Data.Aeson (toJSON)

import Control.Monad.Trans (lift)

modulePG :: (Monad m) => Repository -> InstanceNode -> PG m (ModuleName,ModuleNode)
modulePG repository instancenode = do
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

