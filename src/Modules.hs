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
    Target(Target),TargetType(LibraryTarget),
    InstanceNode,Instance(Instance),
    ModuleNode,Module(Module),ModuleName,ModuleAST,
    FinalizedPackageDescription,TargetSection(LibrarySection))

import Database.PipesGremlin (
    PG,previousLabeled,followingLabeled,nodeProperty,gather,scatter)
import Web.Neo (
    NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)

import Data.Aeson (toJSON)

import Distribution.PackageDescription (
    library,libModules,libBuildInfo,cppOptions,hsSourceDirs)
import Distribution.ModuleName (toFilePath)

import Control.Error (
    runEitherT,EitherT,left,
    runMaybeT,hoistMaybe)

import Data.Map ((!))
import System.Directory (doesFileExist)

import Control.Monad (mzero,forM,filterM)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO,liftIO)

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

    let (Instance (Target variant@(Variant version _) targettype _) _) = inst

    finalizedPackageDescription <- getFinalizedPackageDescription repository variant >>= hoistMaybe
    targetSection <- return (library finalizedPackageDescription) >>= hoistMaybe >>= return . LibrarySection

    let modulenames = enumModuleNames targetSection

    forM modulenames (\modulename -> runEitherT (do

        rawmodulefile <- lookupModuleName repository version targetSection modulename
        modulefile <- preprocess (preprocessorflags targettype finalizedPackageDescription) rawmodulefile
        moduleast <- parse modulefile
        return (Module inst modulename moduleast))))

data ModuleError =
    ModuleFileNotFound |
    MultipleModuleFilesFound
      deriving (Show,Read)

type PreprocessorFlags = [String]

data ModuleFile = ModuleFile

enumModuleNames :: TargetSection -> [ModuleName]
enumModuleNames (LibrarySection librarySection) = libModules librarySection

lookupModuleName :: (MonadIO m) => Repository -> Version -> TargetSection -> ModuleName -> EitherT ModuleError m FilePath
lookupModuleName repository version (LibrarySection librarySection) modulename = do
    let sourcedirs = hsSourceDirs (libBuildInfo librarySection)
        (Version (Package packagename) versionnumber) = version
        packagepath = repository ! packagename ! versionnumber
        potentialPaths = do
            directory <- sourcedirs
            extension <- [".hs",".lhs"]
            return (packagepath ++ directory ++ "/" ++ toFilePath modulename ++ extension)
    modulepaths <- liftIO (filterM doesFileExist potentialPaths)
    case modulepaths of
        [] -> left ModuleFileNotFound
        [modulepath] -> return modulepath
        _ -> left MultipleModuleFilesFound

preprocess :: PreprocessorFlags -> FilePath -> EitherT ModuleError m ModuleFile
preprocess = undefined

parse :: ModuleFile -> EitherT ModuleError m ModuleAST
parse = undefined

preprocessorflags :: TargetType -> FinalizedPackageDescription -> PreprocessorFlags
preprocessorflags LibraryTarget finalizedPackageDescription =
    maybe [] (cppOptions . libBuildInfo) (library finalizedPackageDescription)

insertModule :: (Monad m) => ModuleName -> ModuleAST -> InstanceNode -> NeoT m ModuleNode
insertModule modulename moduleast instancenode = do
    modulenode <- newNode
    addNodeLabel "Module" modulenode
    setNodeProperty "modulename" (toJSON (show modulename)) modulenode
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

finalizationError :: (Monad m) => InstanceNode -> NeoT m ()
finalizationError instancenode = do
    finalizationerrornode <- newNode
    addNodeLabel "Error" finalizationerrornode
    addNodeLabel "FinalizationError" finalizationerrornode
    _ <- newEdge "ERROR" instancenode finalizationerrornode
    return ()

