{-# LANGUAGE OverloadedStrings #-}
module Targets where

import Types (
    Repository,
    Package(Package),PackageNode,
    Version(Version),VersionNumber,VersionNode,
    Variant(Variant),VariantNode,
    PackageDescription,Configuration(Configuration),
    FinalizedPackageDescription,
    Target(Target),TargetNode,
    TargetType(LibraryTarget),PackageDependency)
import Variants (loadPackageDescription)
import Packages (insertPackage)

import Web.Neo (NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)
import Database.PipesGremlin (PG,scatter,gather,has,strain,nodesByLabel,nodeProperty)

import Data.Aeson (toJSON,fromJSON)

import Data.Version (showVersion)
import qualified Data.Version as V (Version(Version))
import Distribution.PackageDescription (
    GenericPackageDescription,FlagAssignment,
    library,targetBuildDepends,libBuildInfo,buildDepends)
import qualified Distribution.PackageDescription as Finalized (PackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Verbosity (silent)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import Distribution.Package (Dependency(Dependency),PackageName(PackageName))

import Control.Monad (forM,guard,(>=>))
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans (lift)

import Data.Map (keys,(!))
import Data.Maybe (maybeToList)
import Data.Text (pack)
import Data.List (nub)

targetPG :: (MonadIO m) => Repository -> (Variant,VariantNode) -> PG m (Target,TargetNode)
targetPG repository (variant,variantnode) = do

    target@(Target _ targettype dependencies) <- targets repository variant >>= scatter

    targetnode <- lift (insertTarget targettype variantnode)
    forM dependencies (findOrCreateDependency >=> lift . (newEdge "PACKAGEDEPENDENCY" targetnode))

    return (target,targetnode)

targets :: (MonadIO m) => Repository -> Variant -> m [Target]
targets repository variant@(Variant version configuration) = do
    packagedescription <- loadPackageDescription repository version
    return (maybeToList (do
        dependencies <- finalize configuration packagedescription >>= libraryDependencies
        return (Target variant LibraryTarget dependencies)))

finalize :: Configuration -> PackageDescription -> Maybe FinalizedPackageDescription
finalize configuration packagedescription = do
    let Configuration flagassignment platform compiler = configuration
        eitherPackageDescription = finalizePackageDescription
            flagassignment
            (const True)
            platform
            compiler
            []
            packagedescription
    (finalizedPackageDescription,flagassignment') <- eitherToMaybe eitherPackageDescription
    guard (flagassignment == flagassignment')
    return finalizedPackageDescription

eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe = either (const Nothing) Just

libraryDependencies :: FinalizedPackageDescription -> Maybe [PackageDependency]
libraryDependencies finalizedPackageDescription = do
    lib <- library finalizedPackageDescription
    let cabalDependencies = targetBuildDepends (libBuildInfo lib) ++ buildDepends finalizedPackageDescription
    return (nub (do
        Dependency (PackageName packagename) _ <- cabalDependencies
        return packagename))

insertTarget :: (Monad m) => TargetType -> VariantNode -> NeoT m TargetNode
insertTarget targettype variantnode = do
    targetnode <- newNode
    addNodeLabel "Target" targetnode
    setNodeProperty "targettype" (toJSON (show targettype)) targetnode
    newEdge "TARGET" variantnode targetnode
    return targetnode

findOrCreateDependency :: (Monad m) => PackageDependency -> PG m PackageNode
findOrCreateDependency packagename = do
    packages <- gather (findPackage packagename)
    case packages of
        []            -> lift (insertPackage packagename)
        [packagenode] -> return packagenode
        packagenodes  -> error "Multiple packagenodes with the same packagename!"

findPackage :: (Monad m) => String -> PG m PackageNode
findPackage packagename =
    nodesByLabel "Package" >>=
    has (nodeProperty "packagename" >=> strain (== (toJSON packagename)))
