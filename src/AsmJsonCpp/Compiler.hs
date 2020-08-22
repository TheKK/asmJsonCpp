{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module provides functions to generate 'CppType', 'CppFn', 'CppExpr' and
-- 'CppStmt' from 'AsmJson'.
module AsmJsonCpp.Compiler
  ( compileToCppFn,
    compileToResultTypes,
    compileToJSONTypeCheck,
    compileToJSONGetter,
  )
where

import AsmJsonCpp.Asm
import AsmJsonCpp.CppExpr
import AsmJsonCpp.Internal.List
import AsmJsonCpp.TypeCheck
import qualified Data.Text.Lazy as L
import RIO
import qualified RIO.NonEmpty as N

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
    outType = N.head $ compileToResultTypes asm cvRef
    outName = "out"

compileToResultTypes :: AsmJson -> CppCV -> N.NonEmpty CppType
compileToResultTypes AsInt cv = pure $ CppTypeNormal cv "int"
compileToResultTypes AsString cv = pure $ CppTypeNormal cv "std::string"
compileToResultTypes (AsObj (AtField _ asm)) cv = compileToResultTypes asm cv
compileToResultTypes (AsObj (FieldsToStruct name fields)) cv =
  thisType N.:| restTypes
  where
    thisType = CppTypeStruct cv name rootOfRestTypes
    rootOfRestTypes = fmap (second N.head) nameAndtypeOfFields
    restTypes = toList . snd =<< nameAndtypeOfFields
    nameAndtypeOfFields =
      (fmap . second) (\asm -> compileToResultTypes asm cvNone) fields
compileToResultTypes (AsArray (AtNth _ asm)) cv = compileToResultTypes asm cv
compileToResultTypes (AsArray (EachElement asm)) cv =
  thisType N.:| toList restTypes
  where
    thisType = CppTypeGeneric cv "std::vector" [rootOfRestTypes]
    rootOfRestTypes = N.head $ restTypes
    restTypes = compileToResultTypes asm cvNone
compileToResultTypes (AsArray (IndexesToStruct name iAndAsms)) cv =
  thisType N.:| restTypes
  where
    thisType = CppTypeStruct cv name $ fmap (\(_i, n, ty) -> (n, ty)) rootOfRestTypes
    rootOfRestTypes = fmap (second N.head) nameAndtypeOfFields
    restTypes = toList . trd =<< nameAndtypeOfFields
    nameAndtypeOfFields =
      (fmap . second) (\asm -> compileToResultTypes asm cvNone) iAndAsms

trd :: (a, b, c) -> c
trd (_, _, c) = c

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
compileToJSONGetter (AsObj (FieldsToStruct name fs)) expr =
  EWorkaround $
    [name <> " {"]
      <> foo
      <> ["}"]
  where
    foo :: [L.Text]
    foo = concatMap (appendOnLast "," . cppExprRenderMulti . EIndent . compile) $ fs

    compile (f, asm) = EIndexOperator expr (EStringLiteral f) & compileToJSONGetter asm
compileToJSONGetter (AsArray (EachElement asm)) expr =
  EWorkaround $
    []
      <> [ "[&] {",
           "  auto ret = std::vector<" <> retTypeText <> ">{};",
           "  for (const auto& v : " <> expr' <> ") {",
           "    ret.emplace_back("
         ]
      <> fmap ("      " <>) vGetter
      <> [ "    );",
           "  }",
           "  return ret;",
           "}()"
         ]
  where
    retTypeText = cppTypeRender $ N.head $ compileToResultTypes asm cvNone
    expr' = cppExprRender expr
    vGetter = cppExprRenderMulti $ compileToJSONGetter asm $ EVarLiteral "v"
compileToJSONGetter (AsArray (AtNth n asm)) expr =
  compileToJSONGetter asm $ EIndexOperator expr (ENumberLiteral n)
compileToJSONGetter (AsArray (IndexesToStruct name iAndAsms)) expr =
  EWorkaround $
    [name <> " {"]
      <> fields
      <> ["}"]
  where
    fields :: [L.Text]
    fields = concatMap (appendOnLast "," . cppExprRenderMulti . EIndent . compile) $ iAndAsms

    compile (i, _f, asm) = EIndexOperator expr (ENumberLiteral i) & compileToJSONGetter asm
