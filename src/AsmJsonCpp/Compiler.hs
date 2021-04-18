{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module provides functions to generate 'CppType', 'CppFn', 'CppExpr' and
-- 'CppStmt' from 'AsmJson'.
module AsmJsonCpp.Compiler
  ( compileToFullCppSourceCode,
    compileToCppFn,
    compileToResultTypes,
    compileToJSONTypeCheck,
    compileToJSONGetter,
  )
where

import AsmJsonCpp.Asm
import AsmJsonCpp.CppExpr
import AsmJsonCpp.TypeCheck
import qualified Data.Text.Lazy as L
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import RIO
import qualified RIO.NonEmpty as N

-- data CppFn = CppFn FunctionName [(CppType, L.Text)] CppType [CppStmt]
compileToFullCppDoc :: AsmJson -> Doc ann
compileToFullCppDoc asm =
  vsep
    [ typeForwardDecls,
      vsep typeDefs',
      functionBody
    ]
  where
    typeForwardDecls = vsep $ mapMaybe cppTypeRenderForwardDeclaration resultTypes
    typeDefs' = case typeDefs of
      [] ->
        [ "// Wow, primitive types rocks right?",
          "// Let's use them everywhere and get confused.",
          "// It's string! It's user name! It's email address as well!! What a lovely day."
        ]
      defs -> defs
    functionBody = cppFnRender . compileToCppFn "from_json" $ asm
    typeDefs = reverse $ mapMaybe cppTypeRenderDefinition resultTypes

    resultTypes = toList $ compileToResultTypes asm cvNone

compileToFullCppSourceCode :: AsmJson -> L.Text
compileToFullCppSourceCode =
  renderLazy
    . layoutPretty (LayoutOptions (AvailablePerLine 100 1))
    . compileToFullCppDoc

compileToCppFn :: L.Text -> AsmJson -> CppFn
compileToCppFn fnName asm =
  CppFn
    fnName
    args
    (CppTypeNormal cvNone "bool")
    [ compileToJSONTypeCheck asm inExpr,
      SMutAssign outExpr $ compileToJSONGetter asm inExpr,
      SReturn $ EBoolLiteral True
    ]
  where
    args = [(inType, inName), (outType, outName)]

    inExpr = EVarLiteral inName
    inType = CppTypeNormal (cvConst <> cvRef) "Json::Value"
    inName = "jsVal"

    outExpr = EVarLiteral outName
    outType = N.head $ compileToResultTypes asm cvRef
    outName = "out"

compileToResultTypes :: AsmJson -> CppCV -> N.NonEmpty CppType
compileToResultTypes AsInt cv = pure $ CppTypeNormal cv "int"
compileToResultTypes AsBool cv = pure $ CppTypeNormal cv "bool"
compileToResultTypes AsString cv = pure $ CppTypeNormal cv "std::string"
compileToResultTypes (AsObj (AtField _ asm)) cv = compileToResultTypes asm cv
compileToResultTypes (AsObj (FieldsToStruct name fields)) cv =
  thisType N.:| restTypes
  where
    thisType = CppTypeStruct cv name rootOfRestTypes
    rootOfRestTypes = fmap (second N.head) nameAndtypeOfFields
    restTypes = toList . snd =<< nameAndtypeOfFields
    nameAndtypeOfFields =
      (fmap . second) (`compileToResultTypes` cvNone) fields
compileToResultTypes (AsArray (AtNth _ asm)) cv = compileToResultTypes asm cv
compileToResultTypes (AsArray (EachElement asm)) cv =
  thisType N.:| toList restTypes
  where
    thisType = CppTypeGeneric cv "std::vector" [rootOfRestTypes]
    rootOfRestTypes = N.head restTypes
    restTypes = compileToResultTypes asm cvNone
compileToResultTypes (AsArray (IndexesToStruct name iAndAsms)) cv =
  thisType N.:| restTypes
  where
    thisType = CppTypeStruct cv name $ fmap (\(_i, n, ty) -> (n, ty)) rootOfRestTypes
    rootOfRestTypes = fmap (second N.head) nameAndtypeOfFields
    restTypes = toList . trd =<< nameAndtypeOfFields
    nameAndtypeOfFields =
      (fmap . second) (`compileToResultTypes` cvNone) iAndAsms

trd :: (a, b, c) -> c
trd (_, _, c) = c

compileToJSONTypeCheck :: AsmJson -> CppExpr -> CppStmt
compileToJSONTypeCheck asm expr = SIf checksExpr [SReturn $ EBoolLiteral False]
  where
    checksExpr = notExpr $ compileTypeChecks $ typeCheck asm expr
    notExpr e = EFunctionCall "!" [e]

compileToJSONGetter :: AsmJson -> CppExpr -> CppExpr
compileToJSONGetter AsInt expr = EMethodCall expr "asInt" []
compileToJSONGetter AsBool expr = EMethodCall expr "asBool" []
compileToJSONGetter AsString expr = EMethodCall expr "asString" []
compileToJSONGetter (AsObj (AtField f asm)) expr =
  EIndexOperator expr (EStringLiteral f)
    & compileToJSONGetter asm
compileToJSONGetter (AsObj (FieldsToStruct name fs)) expr =
  EListInitialization (Just $ CppTypeNormal cvNone name) $ fmap compile fs
  where
    compile (f, asm) = EIndexOperator expr (EStringLiteral f) & compileToJSONGetter asm
compileToJSONGetter (AsArray (EachElement asm)) expr =
  EIIFE
    [ SVarDeclWithInit auto varRet (EListInitialization (vectorTyOf retTy) []),
      SRangeFor constRefAuto "v" expr $
        [ SJustExpr $ EMethodCall ret "emplace_back" [retGetter]
        ],
      SReturn ret
    ]
  where
    auto = CppTypeNormal cvNone "auto"
    constRefAuto = CppTypeNormal (cvConst <> cvRef) "auto"
    varRet = "ret" :: L.Text
    ret = EVarLiteral varRet
    vectorTyOf ty = Just $ CppTypeGeneric cvNone "std::vector" [ty]
    retTy = N.head $ compileToResultTypes asm cvNone
    retGetter = compileToJSONGetter asm $ EVarLiteral "v"
compileToJSONGetter (AsArray (AtNth n asm)) expr =
  compileToJSONGetter asm $ EIndexOperator expr (ENumberLiteral n)
compileToJSONGetter (AsArray (IndexesToStruct name iAndAsms)) expr =
  EListInitialization (Just $ CppTypeNormal cvNone name) $ fmap compile iAndAsms
  where
    compile (i, _f, asm) = EIndexOperator expr (ENumberLiteral i) & compileToJSONGetter asm
