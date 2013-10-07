{-# LANGUAGE OverloadedStrings #-}
module Symbols where

import Types
import Modules.JSON ()

import Database.PipesGremlin (PG,scatter)
import Web.Neo (NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)

import Language.Haskell.Names.SyntaxUtils (splitDeclHead,nameToString)
import Language.Haskell.Names.GetBound (getBound)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalSymbolTable (empty)
import Language.Haskell.Exts.Annotated (Decl(..),DeclHead,Name,SrcSpanInfo)

import Control.Monad.Trans (lift)

symbolPG :: (Monad m) => (Declaration,DeclarationNode) -> PG m (Symbol,SymbolNode)
symbolPG (declaration,declarationnode) = do

    symbol@(Symbol _ symbolname) <- scatter (symbols declaration)

    symbolnode <- lift (insertSymbol symbolname declarationnode)

    return (symbol,symbolnode)

symbols :: Declaration -> [Symbol]
symbols declaration@(Declaration _ declarationast) = do
    symbolname <- typesymbols declarationast ++ valuesymbols declarationast
    return (Symbol declaration (nameToString symbolname))

typesymbols :: DeclarationAST -> [Name ()]
typesymbols = maybe [] ((:[]) . fst . splitDeclHead) . getDeclHead

valuesymbols :: DeclarationAST -> [Name ()]
valuesymbols = getBound (GlobalSymbolTable.empty)

getDeclHead :: Decl l -> Maybe (DeclHead l)
getDeclHead (TypeDecl _ dhead _) = Just dhead
getDeclHead (TypeFamDecl _ dhead _) = Just dhead
getDeclHead (DataDecl _ _ _ dhead _ _) = Just dhead
getDeclHead (GDataDecl _ _ _ dhead _ _ _) = Just dhead
getDeclHead (DataFamDecl _ _ dhead _) = Just dhead
getDeclHead (ClassDecl _ _ dhead _ _) = Just dhead
getDeclHead _ = Nothing

insertSymbol :: (Monad m) => SymbolName -> DeclarationNode -> NeoT m SymbolNode
insertSymbol symbolname declarationnode = do
    symbolnode <- newNode
    addNodeLabel "Symbol" symbolnode
    setNodeProperty "symbolname" symbolname symbolnode
    _ <- newEdge "DECLARATION" declarationnode symbolnode
    return symbolnode



