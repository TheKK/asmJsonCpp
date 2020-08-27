{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module provides tools to check shape of JSON value before using them.
--
-- There're some reasons for you to do these checks:
--
--   * Programing language. You just have to, that's how they works, like Go.
--   * Libraries design. You just have to, you have no choice.
--   * Broekn patch. Someone likes to check everything by hand instead by systematic method.
--     And they patch the library you use into "bad shape".
--
-- If you encounter any of reason above, I feel really sorry for you.
--
-- = Relationships between TypeCheck, AsmJsonCpp and CppExpr
--
-- From the perspective of developers, it's like:
--
-- @ AsmJsonCpp -> TypeCheck -> CppExpr@
--
-- From the perspective of regular user, it's simpler:
--
-- @AsmJsonCpp -> CppExpr@
--
-- you don't even needs to know about what 'TypeCheck' is.
module AsmJsonCpp.TypeCheck
  ( TypeCheck (..),
    compileTypeCheck,
    compileTypeChecks,
    typeCheck,
  )
where

import AsmJsonCpp.Asm
import AsmJsonCpp.CppExpr
import RIO

-- | Describe what 'CppExpr' should be checked.
data TypeCheck
  = ShouldBeInt CppExpr
  | ShouldBeBool CppExpr
  | ShouldBeString CppExpr
  | ShouldBeObj CppExpr
  | ShouldBeMember CppExpr CppExpr
  | ShouldBeArr CppExpr
  | ShouldBeAllChecked CppExpr [CppExpr -> [TypeCheck]]
  | ShouldNthBeChecked Int CppExpr [CppExpr -> [TypeCheck]]
  | -- Group all 'TypeCheck's into one
    ShouldAllBeChecked [TypeCheck]

compileTypeChecks :: [TypeCheck] -> CppExpr
compileTypeChecks [] = EBoolLiteral True
compileTypeChecks cs = cppAndAll . fmap compileTypeCheck $ cs

compileTypeCheck :: TypeCheck -> CppExpr
compileTypeCheck (ShouldBeInt expr) = EMethodCall expr "isInt" []
compileTypeCheck (ShouldBeBool expr) = EMethodCall expr "isBool" []
compileTypeCheck (ShouldBeString expr) = EMethodCall expr "isString" []
compileTypeCheck (ShouldBeObj expr) = EMethodCall expr "isObject" []
compileTypeCheck (ShouldBeArr expr) = EMethodCall expr "isArray" []
compileTypeCheck (ShouldBeMember expr fieldExpr) = EMethodCall expr "isMember" [fieldExpr]
compileTypeCheck (ShouldAllBeChecked cs) = EParentheses $ compileTypeChecks cs
compileTypeCheck (ShouldNthBeChecked nth expr checks) =
  cppAndAll $ nthExistsExpr : fmap compileTypeCheck checks'
  where
    nthExistsExpr = EMethodCall expr "isValidIndex" [ENumberLiteral nth]
    checks' = ($ nthExpr) =<< checks
    nthExpr = EIndexOperator expr (ENumberLiteral nth)
compileTypeCheck (ShouldBeAllChecked expr checks) =
  -- TODO EWorkaround should be removed in the future.
  -- TODO Use 'all_of' to do this
  EWorkaround
    [ "[&]{"
        -- TODO Variable should change for nested case. Even though shadowing is allowed.
        <> (" for (const auto& x : " <> cppExprRender expr <> ") { ")
        <> ("if (!(" <> (cppExprRender . compileTypeChecks $ checks') <> ")) { return false; }")
        <> "} "
        <> "return true; }()"
    ]
  where
    checks' = ($ EVarLiteral "x") =<< checks

typeCheck ::
  AsmJson ->
  -- | Expression to be checked.
  CppExpr ->
  [TypeCheck]
typeCheck AsInt expr = pure . ShouldBeInt $ expr
typeCheck AsBool expr = pure . ShouldBeBool $ expr
typeCheck AsString expr = pure . ShouldBeString $ expr
typeCheck (AsObj obj) expr = ShouldBeObj expr : typeCheckObj obj expr
typeCheck (AsArray arr) expr = ShouldBeArr expr : typeCheckArr arr expr

typeCheckArr :: AsmArray -> CppExpr -> [TypeCheck]
typeCheckArr (EachElement as) expr = pure $ ShouldBeAllChecked expr $ [typeCheck as]
typeCheckArr (AtNth nth as) expr =
  pure $ ShouldNthBeChecked nth expr $ [typeCheck as]
typeCheckArr (IndexesToStruct _ iAndAsms) expr =
  fmap (\(i, _name, asm) -> ShouldAllBeChecked $ typeCheckArr (AtNth i asm) expr) iAndAsms

typeCheckObj :: AsmObj -> CppExpr -> [TypeCheck]
typeCheckObj (AtField f as) expr = checkIsMember : typeCheck as atExpr
  where
    checkIsMember = ShouldBeMember expr $ EStringLiteral f
    atExpr = EIndexOperator expr (EStringLiteral f)
typeCheckObj (FieldsToStruct _ fs) expr =
  (pure . ShouldAllBeChecked . flip typeCheckObj expr . uncurry AtField) =<< fs
