module Lexer (lexTokens) where

import SLToken
import Data.Void
import Data.Char (isAlpha, isAlphaNum)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Função principal que executa o lexer
lexTokens :: String -> String -> Either (ParseErrorBundle String Void) [SLToken]
lexTokens content filename = runParser (sc *> many pToken <* eof) filename content

-- Função que remove espaços e comentarios
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- Ordem dos tokens
pToken :: Parser SLToken
pToken = choice
    [ try pKeyword
      , try pBool
      , try pNumber
      , try pString
      , try pSymbol
      , try pOperator
      , pIdent
    ] <* sc


pKeyword :: Parser SLToken
pKeyword = do
  pos <- getSourcePos
  choice
    [   TkFunc   pos  <$ string "func"
      , TkLet    pos  <$ string "let"
      , TkReturn pos  <$ string "return"
      , TkIf     pos  <$ string "if"
      , TkElse   pos  <$ string "else"
      , TkWhile  pos  <$ string "while"
      , TkForall pos  <$ try (string "forall")
      , TkFor    pos  <$ string "for"
      , TkStruct pos  <$ string "struct"
      , TkNew    pos  <$ string "new"
      , TkVoid   pos  <$ string "void"
      , TkIntType    pos  <$ string "int"
      , TkFloatType  pos  <$ string "float"
      , TkStringType pos  <$ string "string"
      , TkBoolType   pos  <$ string "bool"
    ] <* notFollowedBy alphaNumChar

pBool :: Parser SLToken
pBool = do
  pos <- getSourcePos
  choice
    [   TkBoolLit True  pos  <$ string "true"
      , TkBoolLit False pos  <$ string "false"
    ] <* notFollowedBy alphaNumChar

pNumber :: Parser SLToken
-- try: tenta o parser e se der errado, tenta o próximo, não consome entrada
-- <|> é o operador de alternância
pNumber = try pFloat <|> pInt
  where
    pInt = do
      pos <- getSourcePos
      value <- L.decimal
      return (TkIntLit value pos)

    pFloat = do
      pos <- getSourcePos
      value <- L.float
      return (TkFloatLit value pos)

pString :: Parser SLToken
pString = do
  pos <- getSourcePos
  var <- char '"' *> manyTill L.charLiteral (char '"')
  return (TkStringLit var pos)

pIdent :: Parser SLToken
pIdent = do
  pos <- getSourcePos
  c  <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_')
  let name = c:cs
  return (TkIdent name pos)

pSymbol :: Parser SLToken
pSymbol = do
  pos <- getSourcePos
  choice
    [   TkLParen   pos  <$ char '('
      , TkRParen   pos  <$ char ')'
      , TkLBrace   pos  <$ char '{'
      , TkRBrace   pos  <$ char '}'
      , TkLBracket pos  <$ char '['
      , TkRBracket pos  <$ char ']'
      , TkComma    pos  <$ char ','
      , TkSemicolon pos  <$ char ';'
      , TkColon    pos  <$ char ':'
      , TkDot      pos  <$ char '.'
    ]

pOperator :: Parser SLToken
pOperator = do
  pos <- getSourcePos
  choice
    [   TkArrow     pos  <$ string "->"
      , TkAnd       pos  <$ string "&&"
      , TkOr        pos  <$ string "||"
      , TkLe        pos  <$ string "<="
      , TkGe        pos  <$ string ">="
      , TkEqEq      pos  <$ string "=="
      , TkNeq       pos  <$ try (string "!=")
      , TkNot       pos  <$ char '!'
      , TkAssign    pos  <$ string "="
      , TkPlusPlus  pos  <$ try (string "++")
      , TkPlus      pos  <$ char '+'
      , TkMinusMinus pos  <$ try (string "--")
      , TkMinus     pos  <$ char '-'
      , TkStar      pos  <$ char '*'
      , TkSlash     pos  <$ char '/'
      , TkLt        pos  <$ char '<'
      , TkGt        pos  <$ char '>'
    ]