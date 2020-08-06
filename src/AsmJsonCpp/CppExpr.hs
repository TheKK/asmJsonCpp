{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module AsmJsonCpp.CppExpr
  ( CppType (..),
    cppTypeRender,
    CppCV (..),
    cvNone,
    cvRef,
    CppExpr (..),
    cppExprRender,
    cppAndAll,
    CppStmt (..),
    cppStmtRender,
    CppFn (..),
  )
where

import qualified Data.Text.Lazy as L
import RIO
import RIO.List.Partial

data CppCV = CppCV
  { _cvRef :: Bool
  }

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

cppTypeRender :: CppType -> L.Text
cppTypeRender (CppTypeNormal cv ty) = ty <> cppCVRender cv
cppTypeRender (CppTypeGeneric cv ty args) = ty <> "<" <> args' <> ">" <> cppCVRender cv
  where
    args' = L.intercalate ", " $ fmap cppTypeRender args

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
  | -- | Render anything for you.
    EWorkaround L.Text
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
cppExprRender (EStringLiteral s) = "\"" <> s <> "\""
cppExprRender (EWorkaround s) = s

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

cppStmtRender :: CppStmt -> L.Text
cppStmtRender (SIf expr bodies) =
  L.unlines $
    ["if (" <> cppExprRender expr <> ") {"]
      <> fmap cppStmtRender bodies
      <> ["}"]
cppStmtRender (SMutAssign lexpr rexpr) =
  cppExprRender lexpr <> " = " <> cppExprRender rexpr <> ";"
cppStmtRender (SReturn expr) = "return " <> cppExprRender expr <> ";"

type FunctionName = L.Text

data CppFn = CppFn FunctionName [(CppType, L.Text)] CppType [CppStmt]
