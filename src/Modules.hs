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
    PG,scatter)
import Web.Neo (
    NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)

import Distribution.PackageDescription (
    library,libModules,libBuildInfo,cppOptions,hsSourceDirs)
import Distribution.ModuleName (toFilePath)

import Language.Preprocessor.Cpphs
    (runCpphs,
    defaultCpphsOptions,CpphsOptions(boolopts),
    defaultBoolOptions,BoolOptions(warnings))
import Control.Exception (evaluate)
import Control.DeepSeq (force)

import Language.Haskell.Exts.Annotated (parseFileContentsWithMode)
import Language.Haskell.Exts.Annotated.Fixity (baseFixities)
import Language.Haskell.Exts.Parser (ParseMode(..),defaultParseMode,ParseResult(ParseOk,ParseFailed))

import Control.Error (
    runEitherT,EitherT,left,
    runMaybeT,hoistMaybe,hoistEither,
    scriptIO,fmapLT)

import Data.Map ((!))
import System.Directory (doesFileExist)

import Control.Monad (mzero,forM,filterM)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO,liftIO)

modulePG :: (MonadIO m) => Repository -> InstanceNode -> PG m (Module,ModuleNode)
modulePG repository instancenode = do
    inst <- recoverInstance instancenode
    
    eithermodule <- modules repository inst >>=
        maybe
            (lift (finalizationError instancenode) >> mzero)
            scatter

    either
        (\e -> lift (insertModuleError e instancenode) >> mzero)
        (\modul@(Module _ modulename moduleast) -> do
            modulenode <- lift (insertModule modulename moduleast instancenode)
            return (modul,modulenode))
        eithermodule

modules :: (MonadIO m) => Repository -> Instance -> m (Maybe [Either ModuleError Module])
modules repository inst = runMaybeT (do

    let (Instance (Target variant@(Variant version _) targettype _) _) = inst

    finalizedPackageDescription <- getFinalizedPackageDescription repository variant >>= hoistMaybe
    targetSection <- case targettype of
        LibraryTarget -> hoistMaybe (library finalizedPackageDescription) >>= return . LibrarySection

    let modulenames = enumModuleNames targetSection

    forM modulenames (\modulename -> runEitherT (do

        modulepath <- lookupModuleName repository version targetSection modulename
        modulefile <- preprocess targetSection modulepath
        moduleast <- parse modulepath modulefile
        return (Module inst modulename moduleast))))

data ModuleError =
    ModuleFileNotFound |
    MultipleModuleFilesFound |
    PreprocessorError String |
    ParserError String
      deriving (Show,Read)

type PreprocessorFlags = [String]

type ModuleFile = String

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

preprocess :: (MonadIO m) => TargetSection -> FilePath -> EitherT ModuleError m ModuleFile
preprocess _ modulepath = do
    let cpphsoptions = defaultCpphsOptions {boolopts = booloptions }
        booloptions = defaultBoolOptions {warnings = False}

    scriptIO (do
        rawsource <- readFile modulepath
        sourcecode <- runCpphs cpphsoptions modulepath rawsource
        evaluate (force sourcecode))
            `onException` PreprocessorError

onException :: (Monad m) => EitherT e m a -> (e -> u) -> EitherT u m a
onException = flip fmapLT

parse :: (MonadIO m) => FilePath -> ModuleFile -> EitherT ModuleError m ModuleAST
parse modulepath modulefile = do
    let mode = defaultParseMode {parseFilename = modulepath, fixities = Just baseFixities}

    eitherast <- scriptIO (do
        parseresult <- return (parseFileContentsWithMode mode modulefile)
        case parseresult of
            ParseFailed _ message -> return (Left (ParserError message))
            ParseOk ast -> return (Right ast))
                `onException` ParserError

    hoistEither eitherast

preprocessorflags :: TargetType -> FinalizedPackageDescription -> PreprocessorFlags
preprocessorflags LibraryTarget finalizedPackageDescription =
    maybe [] (cppOptions . libBuildInfo) (library finalizedPackageDescription)

insertModule :: (Monad m) => ModuleName -> ModuleAST -> InstanceNode -> NeoT m ModuleNode
insertModule modulename moduleast instancenode = do
    modulenode <- newNode
    addNodeLabel "Module" modulenode
    setNodeProperty "modulename" (show modulename) modulenode
--    setNodeProperty "moduleast" (show moduleast) modulenode
    _ <- newEdge "MODULE" instancenode modulenode
    return modulenode

insertModuleError :: (Monad m) => ModuleError -> InstanceNode -> NeoT m ()
insertModuleError moduleerror instancenode = do
    moduleerrornode <- newNode
    addNodeLabel "Error" moduleerrornode
    addNodeLabel "ModuleError" moduleerrornode
    setNodeProperty "error" (show moduleerror) moduleerrornode
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

