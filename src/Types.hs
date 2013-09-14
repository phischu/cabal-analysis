module Types where

import Web.Neo (Node)

import Distribution.PackageDescription (FlagAssignment,GenericPackageDescription)
import qualified Distribution.PackageDescription as Cabal (PackageDescription)
import Distribution.System (Platform)
import Distribution.Compiler (CompilerId)
import qualified Language.Haskell.Exts as HSE (Module)

import qualified Data.Version as Version (Version)

import Data.Map (Map)

type Repository = Map PackageName (Map VersionNumber SourcePackage)
type SourcePackage = FilePath

type PackageName   = String
type VersionNumber = Version.Version
type PackageDescription = GenericPackageDescription
data Configuration = Configuration FlagAssignment Platform CompilerId deriving (Show,Read)
type FinalizedPackageDescription = Cabal.PackageDescription
data TargetType = LibraryTarget deriving (Show,Read,Eq)
type PackageDependency = PackageName
type ModuleName = String
type ModuleAST = HSE.Module
type InstanceDependency = InstanceNode

data Package  = Package PackageName deriving (Show,Read)
data Version  = Version Package VersionNumber deriving (Show,Read)
data Variant  = Variant Version Configuration deriving (Show,Read)
data Target   = Target Variant TargetType [PackageDependency] deriving (Show,Read)
data Instance = Instance Target [InstanceDependency]
data Module   = Module Instance ModuleName ModuleAST

type PackageNode  = Node
type VersionNode  = Node
type VariantNode  = Node
type TargetNode   = Node
type InstanceNode = Node
type ModuleNode   = Node
