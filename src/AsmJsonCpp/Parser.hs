{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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

parseAsmJson :: L.Text -> Either (ParseErrorBundle L.Text Void) AsmJson
parseAsmJson = parse (space *> asmJson <* eof) "INPUT"

asmJson :: Parser AsmJson
asmJson = lexeme $ choice [asInt, asString, asObj, asArray]
  where
    asInt = AsInt <$ symbol "AsInt"

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
        <*> choice [eachElement, atNth]
    eachElement =
      EachElement <$ keyword "EachElement"
        <*> asmJson
    atNth =
      AtNth <$ keyword "AtNth"
        <*> nth
        <*> asmJson

fieldName :: Parser L.Text
fieldName = identifier <?> "field name"

structName :: Parser L.Text
structName = identifier <?> "struct name"

identifier :: Parser L.Text
identifier = lexeme (fromString <$> (some $ C.alphaNumChar <|> satisfy (== '_'))) <?> "identifier"

nth :: Parser Int
nth = lexeme Lex.decimal

array :: Parser a -> Parser [a]
array p = between (symbol "[") (symbol "]") $ do
  p `sepEndBy` symbol ","

field :: Parser (L.Text, AsmJson)
field = between (symbol "(") (symbol ")") $ do
  name <- fieldName
  _ <- symbol ","
  asm <- asmJson

  return (name, asm)

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

space :: Parser ()
space =
  Lex.space
    C.space1
    (Lex.skipLineComment "//")
    (Lex.skipBlockComment "/*" "*/")

keyword :: L.Text -> Parser L.Text
keyword t = lexeme . try $ C.string t <* lookAhead C.space1

symbol :: L.Text -> Parser L.Text
symbol t = Lex.symbol space t
