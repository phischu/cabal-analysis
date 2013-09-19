{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Modules.JSON where

import Language.Haskell.Exts.Syntax (
    SpecialCon(..),QName(..),Name(..),IPName(..),QOp(..),Op(..),CName(..),Module(..),ExportSpec(..),
    ImportDecl(..),ImportSpec(..),Assoc(..),Decl(..),Annotation(..),DataOrNew(..),Binds(..),
    IPBind(..),Match(..),QualConDecl(..),ConDecl(..),GadtDecl(..),ClassDecl(..),InstDecl(..),
    BangType(..),Rhs(..),GuardedRhs(..),Type(..),TyVarBind(..),Kind(..),FunDep(..),Asst(..),Literal(..),
    Exp(..),XName(..),XAttr(..),Bracket(..),Splice(..),Safety(..),CallConv(..),ModulePragma(..),
    Activation(..),Rule(..),RuleVar(..),WarningText(..),Pat(..),PXAttr(..),RPatOp(..),RPat(..),
    PatField(..),Stmt(..),QualStmt(..),FieldUpdate(..),Alt(..),GuardedAlts(..),GuardedAlt(..),
    ModuleName(..))
import Language.Haskell.Exts.Annotated.Syntax (Tool(..),Boxed(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..))

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

deriving instance Generic SpecialCon
deriving instance Generic QName
deriving instance Generic Name
deriving instance Generic IPName
deriving instance Generic QOp
deriving instance Generic Op
deriving instance Generic CName
deriving instance Generic Module
deriving instance Generic ExportSpec
deriving instance Generic ImportDecl
deriving instance Generic ImportSpec
deriving instance Generic Assoc
deriving instance Generic Decl
deriving instance Generic Annotation
deriving instance Generic DataOrNew
deriving instance Generic Binds
deriving instance Generic IPBind
deriving instance Generic Match
deriving instance Generic QualConDecl
deriving instance Generic ConDecl
deriving instance Generic GadtDecl
deriving instance Generic ClassDecl
deriving instance Generic InstDecl
deriving instance Generic BangType
deriving instance Generic Rhs
deriving instance Generic GuardedRhs
deriving instance Generic Type
deriving instance Generic TyVarBind
deriving instance Generic Kind
deriving instance Generic FunDep
deriving instance Generic Asst
deriving instance Generic Literal
deriving instance Generic Exp
deriving instance Generic XName
deriving instance Generic XAttr
deriving instance Generic Bracket
deriving instance Generic Splice
deriving instance Generic Safety
deriving instance Generic CallConv
deriving instance Generic ModulePragma
deriving instance Generic Activation
deriving instance Generic Rule
deriving instance Generic RuleVar
deriving instance Generic WarningText
deriving instance Generic Pat
deriving instance Generic PXAttr
deriving instance Generic RPatOp
deriving instance Generic RPat
deriving instance Generic PatField
deriving instance Generic Stmt
deriving instance Generic QualStmt
deriving instance Generic FieldUpdate
deriving instance Generic Alt
deriving instance Generic GuardedAlts
deriving instance Generic GuardedAlt
deriving instance Generic ModuleName
deriving instance Generic Tool
deriving instance Generic Boxed
deriving instance Generic SrcLoc

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
