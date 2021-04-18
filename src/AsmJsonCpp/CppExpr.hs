{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module AsmJsonCpp.CppExpr
  ( CppType (..),
    cppTypeRender,
    cppTypeRenderForwardDeclaration,
    cppTypeRenderDefinition,
    CppCV (..),
    cvNone,
    cvConst,
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
import Data.Text.Prettyprint.Doc
import RIO
import RIO.List.Partial

data CppCV = CppCV
  { _cvConst :: Bool,
    _cvRef :: Bool
  }
  deriving (Show)

instance Semigroup CppCV where
  (CppCV c r) <> (CppCV c' r') = CppCV (c || c') (r || r')

cvNone :: CppCV
cvNone = CppCV False False

cvConst :: CppCV
cvConst = CppCV True False

cvRef :: CppCV
cvRef = CppCV False True

-- | Data represent types in C++.
data CppType
  = -- | Types like 'int', 'Foo', or 'std::string'.
    CppTypeNormal CppCV L.Text
  | -- | Generic type
    CppTypeGeneric CppCV L.Text [CppType]
  | -- | Custom struct type with fields
    CppTypeStruct CppCV L.Text [(L.Text, CppType)]
  deriving (Show)

cppTypeRender :: CppType -> Doc ann
cppTypeRender (CppTypeNormal cv ty) = pretty ty <> cppCVRender cv
cppTypeRender (CppTypeGeneric cv ty args) = pretty ty <> encloseSep "<" ">" ", " (fmap cppTypeRender args) <> cppCVRender cv
cppTypeRender (CppTypeStruct cv name _fields) = pretty name <> cppCVRender cv

-- | This function only render 'CppTypeStruct' since other type should be
--  pre-defined.
cppTypeRenderForwardDeclaration :: CppType -> Maybe (Doc ann)
cppTypeRenderForwardDeclaration (CppTypeStruct _cv name _fields) = Just $ "struct" <+> pretty name <> ";"
cppTypeRenderForwardDeclaration _ = Nothing

-- | This function only render 'CppTypeStruct' since other type should be
--  pre-defined.
cppTypeRenderDefinition :: CppType -> Maybe (Doc ann)
cppTypeRenderDefinition (CppTypeStruct _cv name fields) =
  Just $
    vsep
      [ "struct" <+> pretty name <+> "final {",
        indent 2 $ vsep fields',
        "};"
      ]
  where
    fields' = fmap toFieldText fields
    toFieldText (fieldName, fieldTy) =
      cppTypeRender fieldTy <+> pretty fieldName <> ";"
cppTypeRenderDefinition _ = Nothing

cppCVRender :: CppCV -> Doc ann
cppCVRender (CppCV c r) =
  -- TODO The space before const is a workaround, should use more elegant solution.
  concatWith (<>) [opt c " const", opt r "&"]
  where
    opt True v = v
    opt False _ = ""

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
  | -- TODO Maybe we don't need the Maybe here?
    EListInitialization (Maybe CppType) ~[CppExpr]
  | EIIFE [CppStmt]
  deriving (Show)

cppExprRender :: CppExpr -> Doc ann
cppExprRender (EVarLiteral var) = pretty var
cppExprRender (EInfixFn fn l r) = align $ sep [cppExprRender l <+> pretty fn, cppExprRender r]
cppExprRender (EIndexOperator expr i) = cppExprRender expr <> brackets (align $ cppExprRender i)
cppExprRender (EMethodCall expr method args) =
  hcat
    [ cppExprRender expr,
      "." <> pretty method <> parens (sep $ argsRender args)
    ]
cppExprRender (EFunctionCall fn args) =
  fillCat [pretty fn, parens $ nest 2 $ fillCat $ argsRender args]
cppExprRender (EBoolLiteral True) = "true"
cppExprRender (EBoolLiteral False) = "false"
cppExprRender (ENumberLiteral n) = pretty n
cppExprRender (EParentheses expr) = parens $ cppExprRender expr
cppExprRender (EListInitialization optTy exprs) =
  case exprs of
    [] -> ty <> "{}"
    _ ->
      vIndentBox 2 (ty <+> "{") "}" $
        (<> ",") . cppExprRender <$> exprs
  where
    ty = maybe "" cppTypeRender optTy
cppExprRender (EStringLiteral s) = dquotes $ pretty s
cppExprRender (EIIFE body) =
  vIndentBox 2 "[&] {" "}()" $ cppStmtRender <$> body

-- | Convert [exprA, exprB, exprC] into "exprA && exprB && exprC"
cppAndAll :: Foldable t => t CppExpr -> CppExpr
cppAndAll cs
  | null cs = EBoolLiteral True
  | otherwise = foldr1 (EInfixFn "&&") cs

argsRender :: [CppExpr] -> [Doc ann]
argsRender = punctuate ", " . fmap cppExprRender

data CppStmt
  = SIf CppExpr [CppStmt]
  | SRangeFor CppType L.Text CppExpr [CppStmt]
  | SVarDeclWithInit CppType L.Text CppExpr
  | SMutAssign CppExpr CppExpr
  | SJustExpr CppExpr
  | SReturn CppExpr
  deriving (Show)

cppStmtRender :: CppStmt -> Doc ann
cppStmtRender (SIf expr bodies) =
  vsep
    [ "if (" <> align (cppExprRender expr) <> ") {",
      indent 2 $ fillSep $ fmap cppStmtRender bodies,
      "}"
    ]
cppStmtRender (SRangeFor ty varName expr body) =
  vIndentBox
    2
    ("for (" <> tyDoc <+> varNameDoc <+> ":" <+> exprDoc <> ") {")
    "}"
    $ cppStmtRender <$> body
  where
    tyDoc = cppTypeRender ty
    varNameDoc = pretty varName
    exprDoc = cppExprRender expr
cppStmtRender (SVarDeclWithInit ty varName expr) =
  nest 2 . sep $
    [ cppTypeRender ty <+> pretty varName <+> "=",
      cppExprRender expr <> ";"
    ]
cppStmtRender (SMutAssign lexpr rexpr) =
  nest 2 . sep $
    [ cppExprRender lexpr <+> "=",
      cppExprRender rexpr <> ";"
    ]
cppStmtRender (SJustExpr expr) = cppExprRender expr <> ";"
cppStmtRender (SReturn expr) = sep ["return", cppExprRender expr <> ";"]

type FunctionName = L.Text

data CppFn = CppFn FunctionName [(CppType, L.Text)] CppType [CppStmt]

cppFnRender :: CppFn -> Doc ann
cppFnRender (CppFn fnName args returnType fnBody) =
  vsep
    [ "auto"
        <+> pretty fnName <> parens (fnArgsRender args)
        <+> "->"
        <+> cppTypeRender returnType
        <+> "{",
      indent 2 $ vsep $ fmap cppStmtRender fnBody,
      "}"
    ]

fnArgsRender :: [(CppType, L.Text)] -> Doc ann
fnArgsRender = hcat . punctuate ", " . fmap varDecl
  where
    varDecl (cppType, name) = cppTypeRender cppType <> " " <> pretty name

-- Not the best name though.
vIndentBox :: Int -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
vIndentBox by firstLine lastLine contents =
  hcat . punctuate hardline . join $
    [ [firstLine],
      indent by <$> contents,
      [lastLine]
    ]
