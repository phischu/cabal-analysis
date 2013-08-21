module Repository where

import Types (Repository,PackageName,VersionNumber)

import Data.Version (showVersion)

import Distribution.Hackage.DB (readHackage')

import System.Directory (
    doesFileExist,createDirectoryIfMissing,doesDirectoryExist)
import System.Cmd (rawSystem)

import Control.Monad (when,forM,void)
import Data.Map (Map)
import qualified Data.Map as Map (
    map,keys,filterWithKey,toList,fromList)

packagesDigest :: [PackageName]
packagesDigest = packagesThatMightComeWithGHC

smallPackageSelection :: [PackageName]
smallPackageSelection = ["base","syb","ghc-prim","integer","rts","integer-simple"]

packagesThatMightComeWithGHC :: [PackageName]
packagesThatMightComeWithGHC = smallPackageSelection ++ [
    "array","bytestring","Cabal","containers","deepseq","directory","filepath",
    "haskell2010","haskell98","hpc","old-locale","old-time","pretty","process",
    "template-haskell","time","unix","Win32"]

type Index = Map PackageName [VersionNumber]

loadRepository :: IO Repository
loadRepository = do
    worldIndex <- availablePackages
    let index = pruneIndex packagesDigest worldIndex
    getPackages index

availablePackages :: IO Index
availablePackages = do
    putStrLn "Downloading Index ..."
    exists <- doesFileExist "data/00-index.tar"
    when (not exists) (void (do
        createDirectoryIfMissing True "data/"
        void (rawSystem "wget" [
            "-nv",
            "-O","data/00-index.tar.gz",
            "hackage.haskell.org/packages/archive/00-index.tar"])
        rawSystem "gunzip" ["-f","data/00-index.tar.gz"]))
    hackage <- readHackage' "data/00-index.tar"
    return (Map.map Map.keys hackage)

pruneIndex :: [PackageName] -> Index -> Index
pruneIndex packagenames = Map.filterWithKey (\key _ -> key `elem` packagenames)

getPackages :: Index -> IO Repository
getPackages index = downloadPackages index >> extractPackages index

downloadPackages :: Index -> IO ()
downloadPackages index = do
    putStrLn "Downloading Packages ..."
    void (
        forM (Map.toList index) (\(packagename,versionnumbers) -> do
            forM versionnumbers (\versionnumber -> do
                let directory = archiveFilePath packagename versionnumber
                exists <- doesFileExist directory
                when (not exists) (do
                    createDirectoryIfMissing True "data/archives"
                    void (rawSystem "wget" [
                        "-nv",
                        "-O",directory,
                        packageUrl packagename versionnumber])))))

packageIdentifier :: PackageName -> VersionNumber -> String
packageIdentifier packagename versionnumber = packagename ++ "-" ++ showVersion versionnumber

archiveFilePath :: PackageName -> VersionNumber -> FilePath
archiveFilePath packagename versionnumber = concat [
    "data/archives/",
    packageIdentifier packagename versionnumber,
    ".tar.gz"]

packageUrl :: PackageName -> VersionNumber -> String
packageUrl packagename versionnumber = concat [
    "hackage.haskell.org/packages/archive/",
    packagename,
    "/",
    showVersion versionnumber,
    "/",
    packageIdentifier packagename versionnumber,
    ".tar.gz"]

packageDirectory :: PackageName -> VersionNumber -> FilePath
packageDirectory packagename versionnumber = concat [
    "data/packages/",
    packagename,
    "/",
    packageIdentifier packagename versionnumber,
    "/"]

extractPackages :: Index -> IO Repository
extractPackages index = do
    putStrLn "Extracting Packages ..."
    packageList <- forM (Map.toList index) (\(packagename,versionnumbers) -> do
        versionList <- forM versionnumbers (\versionnumber -> do
            let directory =  packageDirectory packagename versionnumber
                targetDirectory = "data/packages/" ++ packagename
            exists <- doesDirectoryExist directory
            when (not exists) (do
                createDirectoryIfMissing True targetDirectory
                void (rawSystem "tar" [
                    "xzf",
                    archiveFilePath packagename versionnumber,
                    "-C",targetDirectory]))
            return (versionnumber,directory))
        return (packagename,(Map.fromList versionList)))
    return (Map.fromList packageList)



