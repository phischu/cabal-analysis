{-# LANGUAGE OverloadedStrings,TypeFamilies,StandaloneDeriving,FlexibleInstances #-}
module Symbols where

import Types

import Database.PipesGremlin (PG,followingLabeled,nodeProperty,gather,has,strain)
import Web.Neo (NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)

import Language.Haskell.Exts.Annotated (SrcSpanInfo)
import Language.Haskell.Exts.Extension (Language(Haskell2010))

import Language.Haskell.Names (
    computeInterfaces,Symbols(Symbols),
    SymTypeInfo,SymValueInfo,Error,
    OrigName(OrigName),GName(GName))
import Distribution.HaskellSuite.Modules (
    MonadModule(..),ModName(modToString),convertModuleName)

import Data.Set (toList,fromList)
import Data.Either (partitionEithers)
import Text.Read (readMaybe)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT,runReaderT,ask)
import Control.Monad (forM,forM_,(>=>))

symbolPG :: (Monad m) => InstanceNode -> PG m ()
symbolPG instancenode = do

    moduleasts <- return (recoverModuleASTs instancenode)

    errors <- runNeoModuleT instancenode (computeInterfaces Haskell2010 [] moduleasts)

    forM (toList errors) (\err -> lift (insertError err instancenode))

    return ()

recoverModuleASTs :: InstanceNode -> [ModuleAST]
recoverModuleASTs = undefined

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

recoverSymbol :: SymbolNode -> PG m (Either (SymValueInfo OrigName) (SymTypeInfo OrigName))
recoverSymbol = undefined

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
insertValueSymbol = undefined

insertTypeSymbol :: (Monad m) => SymTypeInfo OrigName -> ExportsNode -> NeoT m SymbolNode
insertTypeSymbol = undefined

deriving instance Read GName
deriving instance Read OrigName

{-insertSymbol originmodulename symbolname modulenode = do
    symbolnode <- newNode
    addNodeLabel "Symbol" symbolnode
    setNodeProperty "originmodulename" (show originmodulename) symbolnode
    setNodeProperty "symbolname" symbolname symbolnode
    _ <- newEdge "EXPORT" modulenode symbolnode
    return symbolnode
-}


