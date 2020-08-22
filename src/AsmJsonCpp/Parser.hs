{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AsmJsonCpp.Parser
  ( Parser,
    asmJson,
    parseAsmJson,
  )
where

import AsmJsonCpp.Asm
import Control.Applicative.Combinators hiding (many, skipManyTill, some)
import qualified Data.Text.Lazy as L
import RIO hiding (many, some, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type Parser a = Parsec Void L.Text a

parseAsmJson :: L.Text -> Either (ParseErrorBundle L.Text Void) AsmJson
parseAsmJson = parse (space `skipManyTill` asmJson <* eof) "INPUT"

asmJson :: Parser AsmJson
asmJson = lexeme $ choice [asInt, asString, asObj, asArray]
  where
    asInt = AsInt <$ lastKeyword "AsInt"

    asString = AsString <$ lastKeyword "AsString"

    asObj =
      AsObj <$ keyword "AsObj"
        <*> choice [atField, atFields]
    atField =
      AtField <$ keyword "AtField"
        <*> fieldName
        <*> asmJson
    atFields =
      FieldsToStruct <$ keyword "FieldsToStruct"
        <*> fieldName
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
fieldName = lexeme (fromString <$> (some $ alphaNumChar <|> satisfy (== '_')))

nth :: Parser Int
nth = lexeme Lex.decimal

array :: Parser a -> Parser [a]
array p = between (symbol "[") (symbol "]") $ do
  ps <- many (try $ p <* symbol ",")
  lastP <- optional p
  return $ ps <> maybeToList lastP

field :: Parser (L.Text, AsmJson)
field = between (symbol "(") (symbol ")") $ do
  name <- fieldName
  _ <- symbol ","
  asm <- lexeme $ asmJson

  return (name, asm)

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

keyword :: L.Text -> Parser L.Text
keyword t = lexeme . try $ string t <* lookAhead space1

-- | Like 'token', but don't require trailing space.
lastKeyword :: L.Text -> Parser ()
lastKeyword t = void $ string t

symbol :: L.Text -> Parser L.Text
symbol t = Lex.symbol space t
