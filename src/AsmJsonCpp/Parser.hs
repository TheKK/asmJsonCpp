{-# LANGUAGE OverloadedStrings #-}

module AsmJsonCpp.Parser
  ( Parser,
    asmJson,
    parseAsmJson,
  )
where

import AsmJsonCpp.Asm
import qualified Data.Text.Lazy as L
import RIO hiding (many, some, try)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as Lex

type Parser a = Parsec Void L.Text a

parseAsmJson :: L.Text -> Either L.Text AsmJson
parseAsmJson = first (fromString . errorBundlePretty) . parseAsmJson'

-- | Like 'parseAsmJson' but report error as ParseErrorBundle.
--
-- Using this version require client to use Megaparsec and that's not common.
parseAsmJson' :: L.Text -> Either (ParseErrorBundle L.Text Void) AsmJson
parseAsmJson' = parse (space *> asmJson <* eof) "INPUT"

asmJson :: Parser AsmJson
asmJson = choice [asInt, asBool, asString, asObj, asArray]
  where
    asInt = AsInt <$ symbol "AsInt"

    asBool = AsBool <$ symbol "AsBool"

    asString = AsString <$ symbol "AsString"

    asObj =
      AsObj <$ keyword "AsObj"
        <*> choice [atField, atFields]
    atField =
      AtField <$ keyword "AtField"
        <*> fieldName
        <*> asmJson
    atFields =
      FieldsToStruct <$ keyword "FieldsToStruct"
        <*> structName
        <*> array field

    asArray =
      AsArray <$ keyword "AsArray"
        <*> choice [eachElement, atNth, indexesToStruct]
    eachElement =
      EachElement <$ keyword "EachElement"
        <*> asmJson
    atNth =
      AtNth <$ keyword "AtNth"
        <*> nth
        <*> asmJson
    indexesToStruct =
      IndexesToStruct <$ keyword "IndexesToStruct"
        <*> structName
        <*> array indexField

fieldName :: Parser L.Text
fieldName = lexeme (identifier <* C.space1) <?> "field name"

fieldNameWithoutSpace :: Parser L.Text
fieldNameWithoutSpace = lexeme identifier <?> "field name"

structName :: Parser L.Text
structName = lexeme (identifier <* C.space1) <?> "struct name"

identifier :: Parser L.Text
identifier = fromString <$> some (C.alphaNumChar <|> C.char '_')

nth :: Parser Int
nth = lexeme (Lex.decimal <* C.space1)

array :: Parser a -> Parser [a]
array p =
  between (symbol "[") (symbol "]") $
    p `sepEndBy` symbol ","

field :: Parser (L.Text, AsmJson)
field = between (symbol "(") (symbol ")") $ do
  name <- fieldNameWithoutSpace
  _ <- symbol ","
  asm <- asmJson

  return (name, asm)

indexField :: Parser (Int, L.Text, AsmJson)
indexField = between (symbol "(") (symbol ")") $ do
  i <- index
  _ <- symbol ","
  name <- fieldNameWithoutSpace
  _ <- symbol ","
  asm <- asmJson

  return (i, name, asm)

index :: Parser Int
index = lexeme Lex.decimal

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

space :: Parser ()
space =
  Lex.space
    C.space1
    (Lex.skipLineComment "//")
    (Lex.skipBlockComment "/*" "*/")

keyword :: L.Text -> Parser L.Text
keyword t = lexeme $ C.string t <* C.space1

symbol :: L.Text -> Parser L.Text
symbol = Lex.symbol space
