module Types where

import Web.Neo (Node)

import Distribution.PackageDescription (FlagAssignment)
import Distribution.System (Platform)
import Distribution.Compiler (CompilerId)

import qualified Data.Version as Version (Version)

import Data.Map (Map)

type Repository = Map PackageName (Map VersionNumber SourcePackage)
type SourcePackage = FilePath

type PackageName   = String
type VersionNumber = Version.Version
data Configuration = Configuration FlagAssignment Platform CompilerId deriving (Show,Read)
type ModuleName = String

data Package = Package PackageName deriving (Show,Read)
data Version = Version Package VersionNumber deriving (Show,Read)
data Variant = Variant Version Configuration [Package]
data Instance = Instance Variant [Instance]
data Module = Module Instance ModuleName

type PackageNode = Node
type VersionNode = Node
