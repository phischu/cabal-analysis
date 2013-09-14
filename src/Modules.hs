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
    Target(Target),TargetType,
    InstanceNode,Instance(Instance),
    ModuleNode,Module(Module),ModuleName,ModuleAST,
    FinalizedPackageDescription)

import Database.PipesGremlin (
    PG,previousLabeled,followingLabeled,nodeProperty,gather,scatter)
import Web.Neo (
    NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)

import Data.Aeson (toJSON)

import Control.Error (
    runEitherT,EitherT,left,
    runMaybeT,hoistMaybe)

import Control.Monad (mzero,forM)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)

modulePG :: (MonadIO m) => Repository -> InstanceNode -> PG m (ModuleName,ModuleNode)
modulePG repository instancenode = do
    inst <- recoverInstance instancenode
    
    eithermodule <- modules repository inst >>=
        maybe
            (lift (finalizationError instancenode) >> mzero)
            scatter

    either
        (\e -> lift (insertModuleError e instancenode) >> mzero)
        (\(Module _ modulename moduleast) -> do
            modulenode <- lift (insertModule modulename moduleast instancenode)
            return (modulename,modulenode))
        eithermodule

modules :: (MonadIO m) => Repository -> Instance -> m (Maybe [Either ModuleError Module])
modules repository inst = runMaybeT (do

    let (Instance (Target variant targettype _) _) = inst

    finalizedPackageDescription <- getFinalizedPackageDescription repository variant >>= hoistMaybe

    let modulenames = enumModuleNames targettype finalizedPackageDescription

    forM modulenames (\modulename -> runEitherT (do
        
        rawmodulefile <- lookupModuleName repository modulename
        modulefile <- preprocess (preprocessorflags finalizedPackageDescription) rawmodulefile
        moduleast <- parse modulefile
        return (Module inst modulename moduleast))))

data ModuleError = ModuleError deriving (Show,Read)
type PreprocessorFlags = [String]
data ModuleFile = ModuleFile

enumModuleNames :: TargetType -> FinalizedPackageDescription -> [ModuleName]
enumModuleNames = undefined

lookupModuleName :: Repository -> ModuleName -> EitherT ModuleError m FilePath
lookupModuleName = undefined

preprocess :: PreprocessorFlags -> FilePath -> EitherT ModuleError m ModuleFile
preprocess = undefined

parse :: ModuleFile -> EitherT ModuleError m ModuleAST
parse = undefined

preprocessorflags :: FinalizedPackageDescription -> PreprocessorFlags
preprocessorflags = undefined

insertModule :: (Monad m) => ModuleName -> ModuleAST -> InstanceNode -> NeoT m ModuleNode
insertModule modulename moduleast instancenode = do
    modulenode <- newNode
    addNodeLabel "Module" modulenode
    setNodeProperty "modulename" (toJSON modulename) modulenode
    setNodeProperty "moduleast" (toJSON moduleast) modulenode
    _ <- newEdge "MODULE" instancenode modulenode
    return modulenode

insertModuleError :: (Monad m) => ModuleError -> InstanceNode -> NeoT m ()
insertModuleError moduleerror instancenode = do
    moduleerrornode <- newNode
    addNodeLabel "Error" moduleerrornode
    addNodeLabel "ModuleError" moduleerrornode
    setNodeProperty "error" (toJSON (show moduleerror)) moduleerrornode
    _ <- newEdge "ERROR" instancenode moduleerrornode
    return ()

getFinalizedPackageDescription :: (MonadIO m) =>
    Repository ->
    Variant ->
    m (Maybe FinalizedPackageDescription)
getFinalizedPackageDescription repository variant = do
    let (Variant version configuration) = variant
    packageDescription <- loadPackageDescription repository version
    return (finalize configuration packageDescription)

finalizationError :: InstanceNode -> NeoT m ()
finalizationError = undefined
