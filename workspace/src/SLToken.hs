module SLToken where

import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

data SLToken
  = TkIdent String SourcePos         -- identificadores (variáveis, funções, campos, tipos de usuário)

  -- Literais
  | TkIntLit Integer SourcePos
  | TkFloatLit Double SourcePos
  | TkStringLit String SourcePos
  | TkBoolLit Bool SourcePos

  -- Palavras reservadas
  | TkFunc SourcePos
  | TkLet SourcePos
  | TkReturn SourcePos
  | TkIf SourcePos
  | TkElse SourcePos
  | TkWhile SourcePos
  | TkFor SourcePos
  | TkStruct SourcePos
  | TkNew SourcePos
  | TkForall SourcePos
  | TkVoid SourcePos

  -- Tipos primitivos
  | TkIntType SourcePos
  | TkFloatType SourcePos
  | TkStringType SourcePos
  | TkBoolType SourcePos
  

  -- Símbolos
  | TkLParen SourcePos -- (
  | TkRParen SourcePos -- )
  | TkLBrace SourcePos -- {
  | TkRBrace SourcePos -- }
  | TkLBracket SourcePos -- [
  | TkRBracket SourcePos -- ]
  | TkComma SourcePos -- ,
  | TkSemicolon SourcePos -- ;
  | TkColon SourcePos -- :
  | TkDot SourcePos -- .
  | TkAssign SourcePos -- =
  | TkArrow SourcePos -- ->

  -- Operadores 
  | TkPlus SourcePos           -- +
  | TkPlusPlus SourcePos       -- ++
  | TkMinusMinus SourcePos     -- --, não é necessário, mas já que foi incluido o ++
  | TkMinus SourcePos          -- -
  | TkStar SourcePos           -- *
  | TkSlash SourcePos          -- /
  | TkLt SourcePos             -- <
  | TkGt SourcePos             -- >
  | TkLe SourcePos             -- <=
  | TkGe SourcePos             -- >=
  | TkEqEq SourcePos           -- ==
  | TkNeq SourcePos            -- !=
  | TkAnd SourcePos            -- &&
  | TkOr SourcePos             -- ||
  | TkNot SourcePos            -- !

  deriving (Show, Eq, Ord)


  