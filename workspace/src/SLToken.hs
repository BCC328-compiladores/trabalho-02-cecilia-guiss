{-# LANGUAGE StandaloneDeriving #-}
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
  | TkBackSlash SourcePos -- \
  | TkDoubleColon SourcePos -- ::


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


instance Show SLToken where
  show tok = case tok of
    TkIdent s _      -> s
    TkIntLit n _     -> show n
    TkFloatLit f _   -> show f
    TkStringLit s _  -> show s
    TkBoolLit b _    -> if b then "true" else "false"
    TkFunc _         -> "func"
    TkLet _          -> "let"
    TkReturn _       -> "return"
    TkIf _           -> "if"
    TkElse _         -> "else"
    TkWhile _        -> "while"
    TkFor _          -> "for"
    TkStruct _       -> "struct"
    TkNew _          -> "new"
    TkForall _       -> "forall"
    TkVoid _         -> "void"
    TkIntType _      -> "int"
    TkFloatType _    -> "float"
    TkStringType _   -> "string"
    TkBoolType _     -> "bool"
    TkLParen _       -> "("
    TkRParen _       -> ")"
    TkLBrace _       -> "{"
    TkRBrace _       -> "}"
    TkLBracket _     -> "["
    TkRBracket _     -> "]"
    TkComma _        -> ","
    TkSemicolon _    -> ";"
    TkColon _        -> ":"
    TkDot _          -> "."
    TkAssign _       -> "="
    TkArrow _        -> "->"
    TkBackSlash _    -> "\\"
    TkDoubleColon _  -> "::"
    TkPlus _         -> "+"
    TkPlusPlus _     -> "++"
    TkMinusMinus _   -> "--"
    TkMinus _        -> "-"
    TkStar _         -> "*"
    TkSlash _        -> "/"
    TkLt _           -> "<"
    TkGt _           -> ">"
    TkLe _           -> "<="
    TkGe _           -> ">="
    TkEqEq _         -> "=="
    TkNeq _          -> "!="
    TkAnd _          -> "&&"
    TkOr _           -> "||"
    TkNot _          -> "!"

getTokenPos :: SLToken -> SourcePos
getTokenPos tok = case tok of
  TkIdent _ p      -> p
  TkIntLit _ p     -> p
  TkFloatLit _ p   -> p
  TkStringLit _ p  -> p
  TkBoolLit _ p    -> p
  TkFunc p         -> p
  TkLet p          -> p
  TkReturn p       -> p
  TkIf p           -> p
  TkElse p         -> p
  TkWhile p        -> p
  TkFor p          -> p
  TkStruct p       -> p
  TkNew p          -> p
  TkForall p       -> p
  TkVoid p         -> p
  TkIntType p      -> p
  TkFloatType p    -> p
  TkStringType p   -> p
  TkBoolType p     -> p
  TkLParen p       -> p
  TkRParen p       -> p
  TkLBrace p       -> p
  TkRBrace p       -> p
  TkLBracket p     -> p
  TkRBracket p     -> p
  TkComma p        -> p
  TkSemicolon p    -> p
  TkColon p        -> p
  TkDot p          -> p
  TkAssign p       -> p
  TkArrow p        -> p
  TkBackSlash p    -> p
  TkDoubleColon p  -> p
  TkPlus p         -> p
  TkPlusPlus p     -> p
  TkMinusMinus p   -> p
  TkMinus p        -> p
  TkStar p         -> p
  TkSlash p        -> p
  TkLt p           -> p
  TkGt p           -> p
  TkLe p           -> p
  TkGe p           -> p
  TkEqEq p         -> p
  TkNeq p          -> p
  TkAnd p          -> p
  TkOr p           -> p
  TkNot p          -> p

deriving instance Eq SLToken
deriving instance Ord SLToken


  