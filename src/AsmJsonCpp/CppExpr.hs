{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module AsmJsonCpp.CppExpr
  ( CppType (..),
    cppTypeRender,
    cppTypeRenderDefinition,
    CppCV (..),
    cvNone,
    cvRef,
    CppExpr (..),
    cppExprRender,
    cppAndAll,
    CppStmt (..),
    cppStmtRender,
    CppFn (..),
    cppFnRender,
  )
where

import qualified Data.Text.Lazy as L
import RIO
import RIO.List.Partial

data CppCV = CppCV
  { _cvRef :: Bool
  }
  deriving (Show)

instance Semigroup CppCV where
  (CppCV ref) <> (CppCV ref') = CppCV (ref || ref')

cvNone :: CppCV
cvNone = CppCV False

cvRef :: CppCV
cvRef = CppCV True

-- | Data represent types in C++.
data CppType
  = -- | Types like 'int', 'Foo', or 'std::string'.
    CppTypeNormal CppCV L.Text
  | -- | Generic type
    CppTypeGeneric CppCV L.Text [CppType]
  | -- | Custom struct type with fields
    CppTypeStruct CppCV L.Text [(L.Text, CppType)]
  deriving (Show)

cppTypeRender :: CppType -> L.Text
cppTypeRender (CppTypeNormal cv ty) = ty <> cppCVRender cv
cppTypeRender (CppTypeGeneric cv ty args) = ty <> "<" <> args' <> ">" <> cppCVRender cv
  where
    args' = L.intercalate ", " $ fmap cppTypeRender args
cppTypeRender (CppTypeStruct cv name _fields) = name <> cppCVRender cv

-- | This function only render 'CppTypeStruct' since other type should be
--  pre-defined.
cppTypeRenderDefinition :: CppType -> Maybe L.Text
cppTypeRenderDefinition (CppTypeStruct _cv name fields) =
  Just $
    L.unlines $
      ["struct " <> name <> " final {"]
        <> fmap ("  " <>) fields'
        <> ["};"]
  where
    fields' = fmap toFieldText fields
    toFieldText (fieldName, fieldTy) =
      cppTypeRender fieldTy <> " " <> fieldName <> ";"
cppTypeRenderDefinition _ = Nothing

cppCVRender :: CppCV -> L.Text
cppCVRender (CppCV True) = "&"
cppCVRender (CppCV False) = ""

data CppExpr
  = EVarLiteral L.Text
  | EInfixFn L.Text CppExpr CppExpr
  | EIndexOperator CppExpr CppExpr
  | EMethodCall CppExpr L.Text ~[CppExpr]
  | EFunctionCall L.Text ~[CppExpr]
  | EBoolLiteral Bool
  | EStringLiteral L.Text
  | ENumberLiteral Int
  | EParentheses CppExpr
  | -- | Render anything for you.
    EWorkaround [L.Text]
  deriving (Show)

cppExprRender :: CppExpr -> L.Text
cppExprRender (EVarLiteral var) = var
cppExprRender (EInfixFn fn l r) = cppExprRender l <> " " <> fn <> " " <> cppExprRender r
cppExprRender (EIndexOperator expr i) = cppExprRender expr <> "[" <> cppExprRender i <> "]"
cppExprRender (EMethodCall expr method args) =
  cppExprRender expr <> "." <> method <> "(" <> argsRender args <> ")"
cppExprRender (EFunctionCall fn args) = fn <> "(" <> argsRender args <> ")"
cppExprRender (EBoolLiteral True) = "true"
cppExprRender (EBoolLiteral False) = "false"
cppExprRender (ENumberLiteral n) = L.pack $ show n
cppExprRender (EParentheses expr) = "(" <> cppExprRender expr <> ")"
cppExprRender (EStringLiteral s) = "\"" <> s <> "\""
cppExprRender (EWorkaround s) = L.unlines s

-- | Convert [exprA, exprB, exprC] into "exprA && exprB && exprC"
cppAndAll :: Foldable t => t CppExpr -> CppExpr
cppAndAll cs
  | null cs = EBoolLiteral True
  | otherwise = foldr1 (EInfixFn "&&") cs

argsRender :: [CppExpr] -> L.Text
argsRender = L.intercalate ", " . fmap cppExprRender

data CppStmt
  = SIf CppExpr [CppStmt]
  | SMutAssign CppExpr CppExpr
  | SReturn CppExpr
  | SIndent [CppStmt]

cppStmtRender :: CppStmt -> [L.Text]
cppStmtRender (SIf expr bodies) =
  ["if (" <> cppExprRender expr <> ") {"]
    <> (cppStmtRender . SIndent) bodies
    <> ["}"]
cppStmtRender (SMutAssign lexpr rexpr) =
  [ cppExprRender lexpr <> " = " <> cppExprRender rexpr <> ";"
  ]
cppStmtRender (SReturn expr) =
  [ "return " <> cppExprRender expr <> ";"
  ]
cppStmtRender (SIndent stmts) = fmap ("  " <>) $ cppStmtRender =<< stmts

type FunctionName = L.Text

data CppFn = CppFn FunctionName [(CppType, L.Text)] CppType [CppStmt]

cppFnRender :: CppFn -> L.Text
cppFnRender (CppFn fnName args returnType fnBody) =
  L.unlines $
    ["auto " <> fnName <> "(" <> argsText <> ") -> " <> cppTypeRender returnType <> " {"]
      <> (cppStmtRender =<< fnBody)
      <> ["}"]
  where
    argsText = fnArgsRender args

fnArgsRender :: [(CppType, L.Text)] -> L.Text
fnArgsRender = L.intercalate ", " . fmap varDecl
  where
    varDecl (cppType, name) = cppTypeRender cppType <> " " <> name
