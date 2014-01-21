{-# LANGUAGE OverloadedStrings,TypeFamilies,StandaloneDeriving,FlexibleInstances #-}
module Exports where

import Types

import Database.PipesGremlin (PG,followingLabeled,nodeProperty,gather,has,strain)
import Web.Neo (NeoT,Label,newNode,addNodeLabel,setNodeProperty,newEdge,nodeId)

import Language.Haskell.Exts.Annotated (parseModule,SrcSpanInfo,Assoc(..))
import Language.Haskell.Exts.Extension (Language(Haskell2010))
import Language.Haskell.Exts.Parser (ParseResult(ParseFailed,ParseOk))

import Language.Haskell.Names (
    computeInterfaces,Symbols(Symbols),SymFixity,
    SymValueInfo(..),
    SymTypeInfo(..),
    Error,OrigName(OrigName),GName(GName))
import Distribution.HaskellSuite.Modules (
    MonadModule(..),ModName(modToString),convertModuleName)

import Data.Set (Set,insert,member,toList,fromList)
import Data.Either (partitionEithers)
import Data.Text (Text)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT,runReaderT,ask)
import Control.Monad.Trans.State (StateT,get,put)
import Control.Monad (forM,forM_,(>=>),mzero,guard,when)

exportsPG :: (Monad m) => InstanceNode -> PG (StateT (Set Integer) m) ()
exportsPG instancenode = do

    visitedInstanceNodeIds <- lift ( lift (lift get))
    lift (lift (lift (put (insert (nodeId instancenode) visitedInstanceNodeIds))))

    when (nodeId instancenode `member` visitedInstanceNodeIds) mzero

 --   instancedependencies <- gather (return instancenode >>= followingLabeled "INSTANCEDEPENDENCY")
 --   forM_ instancedependencies exportsPG

    moduleasts <- recoverModuleASTs instancenode

    errors <- runNeoModuleT instancenode (computeInterfaces Haskell2010 [] moduleasts)

    forM (toList errors) (\err -> lift (insertError err instancenode))

    return ()

recoverModuleASTs :: (Monad m) => InstanceNode -> PG m [ModuleAST]
recoverModuleASTs = gather . (followingLabeled "MODULE" >=> nodeProperty "modulesource" >=> (\m ->
    case parseModule m of
        ParseFailed _ _ -> mzero
        ParseOk modul -> return modul))

newtype NeoModuleT m a = NeoModuleT { unNeoModuleT :: ReaderT InstanceNode (PG m) a}

instance (Monad m) => Monad (NeoModuleT m) where
    ma >>= amb = NeoModuleT (unNeoModuleT ma >>= unNeoModuleT . amb)
    return = NeoModuleT . return

instance (Monad m) => MonadModule (NeoModuleT m) where
    type ModuleInfo (NeoModuleT m) = Symbols
    lookupInCache modulename = NeoModuleT (do
        instancenode <- ask
        exportsnodes <- lift (gather (return instancenode >>= instanceModule (modToString modulename) >>= moduleExports))
        case exportsnodes of
            [exportsnode] -> lift (do
                (symvalueinfos,symtypeinfos) <- fmap partitionEithers (gather (
                    exportedSymbol exportsnode >>=
                    recoverSymbol))
                return (Just (Symbols (fromList symvalueinfos) (fromList symtypeinfos))))
            _ -> return Nothing)
    insertInCache modulename symbols = NeoModuleT (do
        instancenode <- ask
        lift (do
            modulenode <- instanceModule (modToString modulename) instancenode
            lift (insertExports modulenode >>= insertSymbols symbols)))
    getPackages   = return []
    readModuleInfo filepaths modulename = error
        ("not implemented readModuleInfo: "++show filepaths++" "++modToString modulename)

instanceModule :: (Monad m) => String -> InstanceNode -> PG m ModuleNode
instanceModule modulenamestring = do
    let modulename = convertModuleName modulenamestring
    followingLabeled "MODULE" >=> has (
        nodeProperty "modulename" >=>
        strain ((==) (show modulename)))

moduleExports :: (Monad m) => ModuleNode -> PG m ExportsNode
moduleExports = followingLabeled "EXPORTS"

exportedSymbol :: (Monad m) => ExportsNode -> PG m SymbolNode
exportedSymbol = followingLabeled "EXPORT"

recoverSymbol :: (Monad m) => SymbolNode -> PG m (Either (SymValueInfo OrigName) (SymTypeInfo OrigName))
recoverSymbol symbolnode = do
    genre <- nodeProperty "genre" symbolnode
    originname <- nodeProperty "originname" symbolnode >>= return . read
    maybefixity <- nodeProperty "maybefixity" symbolnode >>= return . read
    case (genre :: String) of
        "Value" -> do
            return (Left (SymValue originname maybefixity))
        "Method" -> do
            originclassname <- nodeProperty "originclassname" symbolnode >>= return . read
            return (Left (SymMethod originname maybefixity originclassname))
        "Selector" -> do
            origintypename <- nodeProperty "origintypename" symbolnode >>= return . read
            originconstructors <- nodeProperty "originconstructors" symbolnode >>= return . read
            return (Left (SymSelector originname maybefixity origintypename originconstructors))
        "Constructor" -> do
            origintypename <- nodeProperty "origintypename" symbolnode >>= return . read
            return (Left (SymConstructor originname maybefixity origintypename))
        "Type" -> return (Right (SymType originname maybefixity))
        "Newtype" -> return (Right (SymNewType originname maybefixity))
        "TypeFamily" -> return (Right (SymTypeFam originname maybefixity))
        "DataFamily" -> return (Right (SymDataFam originname maybefixity))
        "Class" -> return (Right (SymClass originname maybefixity))

runNeoModuleT :: InstanceNode -> NeoModuleT m a -> PG m a
runNeoModuleT instancenode = flip runReaderT instancenode . unNeoModuleT

insertError :: (Monad m) => Error SrcSpanInfo -> InstanceNode -> NeoT m ()
insertError err instancenode = do
    errornode <- newNode
    addNodeLabel "NameError" errornode
    setNodeProperty "error" (show err) errornode
    _ <- newEdge "NAMEERROR" instancenode errornode
    return ()

insertExports :: (Monad m) => ModuleNode -> NeoT m ExportsNode
insertExports modulenode = do
    exportsnode <- newNode
    addNodeLabel "Exports" exportsnode
    _ <- newEdge "EXPORTS" modulenode exportsnode
    return exportsnode

insertSymbols :: (Monad m) => Symbols -> ExportsNode -> NeoT m ()
insertSymbols (Symbols valuesymbols typesymbols) exportsnode = do
    forM_ (toList valuesymbols) (\valuesymbol -> insertValueSymbol valuesymbol exportsnode)
    forM_ (toList typesymbols) (\typesymbol -> insertTypeSymbol typesymbol exportsnode)

insertValueSymbol :: (Monad m) => SymValueInfo OrigName -> ExportsNode -> NeoT m SymbolNode
insertValueSymbol (SymValue originname maybefixity) =
    insertSymbol originname "Value" maybefixity []
insertValueSymbol (SymMethod originname maybefixity originclassname) =
    insertSymbol originname "Method" maybefixity [("originclassname",show originclassname)]
insertValueSymbol (SymSelector originname maybefixity origintypename originconstructors) =
    insertSymbol originname "Selector" maybefixity [("origintypename",show origintypename),("originconstructors",show originconstructors)]
insertValueSymbol (SymConstructor originname maybefixity origintypename) =
    insertSymbol originname "Constructor" maybefixity [("origintypename",show origintypename)]

insertTypeSymbol :: (Monad m) => SymTypeInfo OrigName -> ExportsNode -> NeoT m SymbolNode
insertTypeSymbol (SymType originname maybefixity) =
    insertSymbol originname "Type" maybefixity []
insertTypeSymbol (SymData originname maybefixity) =
    insertSymbol originname "Data" maybefixity []
insertTypeSymbol (SymNewType originname maybefixity) =
    insertSymbol originname "Newtype" maybefixity []
insertTypeSymbol (SymTypeFam originname maybefixity) =
    insertSymbol originname "TypeFamily" maybefixity []
insertTypeSymbol (SymDataFam originname maybefixity) =
    insertSymbol originname "DataFamily" maybefixity []
insertTypeSymbol (SymClass originname maybefixity) =
    insertSymbol originname "Class" maybefixity []

insertSymbol :: (Monad m) => OrigName -> Label -> Maybe SymFixity -> [(Text,String)] -> ExportsNode -> NeoT m SymbolNode
insertSymbol originname label maybefixity properties exportsnode = do
    symbolnode <- newNode
    addNodeLabel "Symbol" symbolnode
    addNodeLabel label symbolnode
    setNodeProperty "originname" (show originname) symbolnode
    setNodeProperty "maybefixity" (show maybefixity) symbolnode
    forM_ properties (\(key,value) -> setNodeProperty key value symbolnode)
    setNodeProperty "genre" label symbolnode
    _ <- newEdge "EXPORT" exportsnode symbolnode
    return symbolnode

deriving instance Read GName
deriving instance Read OrigName
deriving instance Read (Assoc ())



