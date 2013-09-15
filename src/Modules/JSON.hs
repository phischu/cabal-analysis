{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Modules.JSON where

import Language.Haskell.Exts.Syntax (
    SpecialCon,QName,Name,IPName,QOp,Op,CName,Module,ExportSpec,
    ImportDecl,ImportSpec,Assoc,Decl,Annotation,DataOrNew,Binds,
    IPBind,Match,QualConDecl,ConDecl,GadtDecl,ClassDecl,InstDecl,
    BangType,Rhs,GuardedRhs,Type,TyVarBind,Kind,FunDep,Asst,Literal,
    Exp,XName,XAttr,Bracket,Splice,Safety,CallConv,ModulePragma,
    Activation,Rule,RuleVar,WarningText,Pat,PXAttr,RPatOp,RPat,
    PatField,Stmt,QualStmt,FieldUpdate,Alt,GuardedAlts,GuardedAlt,
    ModuleName)
import Language.Haskell.Exts.Annotated.Syntax (Tool,Boxed)
import Language.Haskell.Exts.SrcLoc (SrcLoc)

import Data.Aeson (ToJSON)


instance ToJSON SpecialCon
instance ToJSON QName
instance ToJSON Name
instance ToJSON IPName
instance ToJSON QOp
instance ToJSON Op
instance ToJSON CName
instance ToJSON Module
instance ToJSON ExportSpec
instance ToJSON ImportDecl
instance ToJSON ImportSpec
instance ToJSON Assoc
instance ToJSON Decl
instance ToJSON Annotation
instance ToJSON DataOrNew
instance ToJSON Binds
instance ToJSON IPBind
instance ToJSON Match
instance ToJSON QualConDecl
instance ToJSON ConDecl
instance ToJSON GadtDecl
instance ToJSON ClassDecl
instance ToJSON InstDecl
instance ToJSON BangType
instance ToJSON Rhs
instance ToJSON GuardedRhs
instance ToJSON Type
instance ToJSON TyVarBind
instance ToJSON Kind
instance ToJSON FunDep
instance ToJSON Asst
instance ToJSON Literal
instance ToJSON Exp
instance ToJSON XName
instance ToJSON XAttr
instance ToJSON Bracket
instance ToJSON Splice
instance ToJSON Safety
instance ToJSON CallConv
instance ToJSON ModulePragma
instance ToJSON Activation
instance ToJSON Rule
instance ToJSON RuleVar
instance ToJSON WarningText
instance ToJSON Pat
instance ToJSON PXAttr
instance ToJSON RPatOp
instance ToJSON RPat
instance ToJSON PatField
instance ToJSON Stmt
instance ToJSON QualStmt
instance ToJSON FieldUpdate
instance ToJSON Alt
instance ToJSON GuardedAlts
instance ToJSON GuardedAlt
instance ToJSON ModuleName
instance ToJSON Tool
instance ToJSON Boxed
instance ToJSON SrcLoc
