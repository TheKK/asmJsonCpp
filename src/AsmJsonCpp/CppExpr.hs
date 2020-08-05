{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module AsmJsonCpp.CppExpr
  ( CppExpr (..),
    cppExprRender,
    cppAndAll,
  )
where

import qualified Data.Text.Lazy as L
import RIO
import RIO.List.Partial

data CppExpr
  = EVarLiteral L.Text
  | EInfixFn L.Text CppExpr CppExpr
  | EIndexOperator CppExpr CppExpr
  | EMethodCall CppExpr L.Text ~[CppExpr]
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
