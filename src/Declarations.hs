{-# LANGUAGE OverloadedStrings #-}
module Declarations where

import Types
import Modules.JSON ()

import Database.PipesGremlin (PG,scatter)
import Web.Neo (NeoT,newNode,addNodeLabel,setNodeProperty,newEdge)

import Language.Haskell.Names.SyntaxUtils (dropAnn)
import qualified Language.Haskell.Exts.Annotated as HSE (Module(Module))

import Control.Monad.Trans (lift)

declarationPG :: (Monad m) => (Module,ModuleNode) -> PG m (Declaration,DeclarationNode)
declarationPG (modul,modulenode) = do

    declaration@(Declaration _ declarationast) <- scatter (declarations modul)

    declarationnode <- lift (insertDeclaration declarationast modulenode)

    return (declaration,declarationnode)

declarations :: Module -> [Declaration]
declarations modul@(Module _ _ (HSE.Module _ _ _ _ decls)) =
    map (Declaration modul) (map dropAnn decls)
declarations _ =
    []

insertDeclaration :: (Monad m) => DeclarationAST -> ModuleNode -> NeoT m DeclarationNode
insertDeclaration declarationast modulenode = do
    declarationnode <- newNode
    addNodeLabel "Declaration" declarationnode
    setNodeProperty "declarationast" (show declarationast) declarationnode
    _ <- newEdge "DECLARATION" modulenode declarationnode
    return declarationnode
