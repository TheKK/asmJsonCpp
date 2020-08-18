{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module provides functions to generate 'CppType', 'CppFn', 'CppExpr' and
-- 'CppStmt' from 'AsmJson'.
module AsmJsonCpp.Compiler
  ( compileToCppFn,
    compileToResultType,
    compileToJSONTypeCheck,
    compileToJSONGetter,
  )
where

import AsmJsonCpp.Asm
import AsmJsonCpp.CppExpr
import AsmJsonCpp.TypeCheck
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.IO
import RIO

compileToCppFn :: L.Text -> AsmJson -> CppFn
compileToCppFn fnName asm =
  CppFn fnName args (CppTypeNormal cvNone "bool") $
    [ compileToJSONTypeCheck asm inExpr,
      SMutAssign outExpr $ compileToJSONGetter asm inExpr,
      SReturn $ EBoolLiteral True
    ]
  where
    args = [(inType, inName), (outType, outName)]

    inExpr = EVarLiteral inName
    inType = CppTypeNormal cvRef "const Json::Value"
    inName = "jsVal"

    outExpr = EVarLiteral outName
    outType = compileToResultType asm cvRef
    outName = "out"

compileToResultType :: AsmJson -> CppCV -> CppType
compileToResultType AsInt cv = CppTypeNormal cv "int"
compileToResultType AsString cv = CppTypeNormal cv "std::string"
compileToResultType (AsObj (AtField _ asm)) cv = compileToResultType asm cv
compileToResultType (AsObj (AtFields fields)) cv = CppTypeStruct cv "T" typeOfFields
  where
    typeOfFields = (fmap . second $ \asm -> compileToResultType asm cvNone) fields
compileToResultType (AsArray (AtNth _ asm)) cv = compileToResultType asm cv
compileToResultType (AsArray (EachElement asm)) cv =
  CppTypeGeneric cv "std::vector" [compileToResultType asm cvNone]

compileToJSONTypeCheck :: AsmJson -> CppExpr -> CppStmt
compileToJSONTypeCheck asm expr = SIf checksExpr [SReturn $ EBoolLiteral False]
  where
    checksExpr = notExpr $ compileTypeChecks $ typeCheck asm expr
    notExpr e = EFunctionCall "!" [e]

compileToJSONGetter :: AsmJson -> CppExpr -> CppExpr
compileToJSONGetter AsInt expr = EMethodCall expr "asInt" []
compileToJSONGetter AsString expr = EMethodCall expr "asString" []
compileToJSONGetter (AsObj (AtField f asm)) expr =
  EIndexOperator expr (EStringLiteral f)
    & compileToJSONGetter asm
compileToJSONGetter (AsObj (AtFields fs)) expr =
  EWorkaround $
    ["{"]
      <> foo
      <> ["}"]
  where
    foo :: [L.Text]
    foo = fmap ((<> ",") . cppExprRender . compile) $ fs

    compile (f, asm) = EIndexOperator expr (EStringLiteral f) & compileToJSONGetter asm
compileToJSONGetter (AsArray (EachElement asm)) expr =
  EWorkaround
    [ "[&] {",
      "  auto ret = std::vector<" <> retTypeText <> ">{};",
      "  for (const auto& v : " <> expr' <> ") {",
      "    ret.emplace_back(" <> vGetter <> ");",
      "  }",
      "  return ret;",
      "}()"
    ]
  where
    retTypeText = cppTypeRender $ compileToResultType asm cvNone
    expr' = cppExprRender expr
    vGetter = cppExprRender $ compileToJSONGetter asm $ EVarLiteral "v"
compileToJSONGetter (AsArray (AtNth n asm)) expr =
  compileToJSONGetter asm $ EIndexOperator expr (ENumberLiteral n)