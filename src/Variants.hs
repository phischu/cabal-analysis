{-# LANGUAGE OverloadedStrings #-}
module Variants where

import Types (
    Repository,
    Package(Package),
    Version(Version),VersionNode,
    Variant(Variant),VariantNode,
    PackageDescription,Configuration(Configuration),
    FinalizedPackageDescription)

import Web.Neo (NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)
import Database.PipesGremlin (PG,scatter)

import Data.Aeson (toJSON)

import qualified Data.Version as V (Version(Version))
import Distribution.PackageDescription (FlagAssignment)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Verbosity (silent)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import Distribution.Package (Dependency)

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans (lift)

import Data.Map ((!))

variantPG :: (MonadIO m) => Repository -> (Version,VersionNode) -> PG m (Variant,VariantNode)
variantPG repository (version,versionnode) = do

    variant@(Variant _ configuration) <- variants repository version >>= scatter

    variantnode <- lift (insertVariant configuration versionnode)

    return (variant,variantnode)

variants :: (MonadIO m) => Repository -> Version -> m [Variant]
variants repository version = do
    description <- loadPackageDescription repository version
    forM (configurations description) (\configuration ->
        return (Variant version configuration))

loadPackageDescription :: (MonadIO m) => Repository -> Version -> m PackageDescription
loadPackageDescription repository version = do
    let (Version (Package packagename) versionnumber) = version
        packagedirectory = repository ! packagename ! versionnumber
        cabalfilepath = packagedirectory ++ packagename ++ ".cabal"
    liftIO (readPackageDescription silent cabalfilepath)

configurations :: PackageDescription -> [Configuration]
configurations packagedescription = case cabalConfigure packagedescription of
    Left _ ->
        []
    Right (_,flagassignment) ->
        [Configuration flagassignment defaultPlatform defaultCompiler]

defaultPlatform :: Platform
defaultPlatform = Platform I386 Linux

defaultCompiler :: CompilerId
defaultCompiler = CompilerId GHC (V.Version [7,6,3] [])

cabalConfigure :: PackageDescription -> Either [Dependency] (FinalizedPackageDescription,FlagAssignment)
cabalConfigure = finalizePackageDescription [] (const True) defaultPlatform defaultCompiler []

insertVariant :: (Monad m) => Configuration -> VersionNode -> NeoT m VariantNode
insertVariant configuration versionnode = do
    variantnode <- newNode
    addNodeLabel "Variant" variantnode
    setNodeProperty "configuration" (toJSON (show configuration)) variantnode
    _ <- newEdge "VARIANT" versionnode variantnode
    return variantnode
