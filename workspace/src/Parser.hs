{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant variable capture" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use fromMaybe" #-}
module Parser (parseSL, Definition(..), Func(..), Struct(..), Stmt(..), Expr(..), GlobalLet(..)) where

import SLToken

import Text.Megaparsec
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Data.Void
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Data.Proxy

type Parser = Parsec Void [SLToken]

instance VisualStream [SLToken] where
  showTokens _ = unwords . map show . NE.toList

instance TraversableStream [SLToken] where
  reachOffset o pst =
    let (before, after) = splitAt (o - pstateOffset pst) (pstateInput pst)
        newPos = case after of
          [] -> pstateSourcePos pst
          (t:_) -> getTokenPos t
    in (Nothing, pst { pstateInput = after, pstateOffset = o, pstateSourcePos = newPos })

-- Função principal do parser
parseSLMP :: [SLToken] -> Either (ParseErrorBundle [SLToken] Void) [Definition]
parseSLMP = runParser (many pTopItem <* eof) ""

pTopItem :: Parser Definition
pTopItem = try (DefStruct <$> pStruct) <|> (DefFunc <$> pFunc) <|> (DefGlobalLet <$> pGlobalLet)

pStruct :: Parser Struct
pStruct = do
  _ <- kwStruct
  name <- pIdent
  _ <- symLBrace
  fields <- many pStructField
  _ <- symRBrace
  return (Struct name fields) 

pStructField :: Parser (String, String)
pStructField = do
  name <- pIdent
  _ <- symColon
  t <- pType
  _ <- symSemicolon
  return (name, t)

-- AST
data Expr
  = EInt Integer
  | EFloat Double
  | EString String
  | EVar String
  | ECall String [Expr]
  | EArray [Expr]
  | ENew String Expr
  | EBin String Expr Expr
  | EPost Expr String
  | EPre String Expr
  | ELambda Func -- Func anônima
  
  deriving (Show, Eq)

data Stmt
  = SReturn Expr
  | SLet String (Maybe String) (Maybe Expr)
  | SIf Expr [Stmt] (Maybe [Stmt])
  | SWhile Expr [Stmt]
  | SFor (Maybe Stmt) Expr (Maybe Expr) [Stmt]
  | SExpr Expr
  | SDef Definition
  deriving (Show, Eq)

data Definition
  = DefFunc Func
  | DefStruct Struct
  | DefGlobalLet GlobalLet
  deriving (Show, Eq)

data GlobalLet = GlobalLet
  { lName :: String
  , ltype :: Maybe String
  , lval :: Maybe Expr
  } deriving (Show, Eq)

data Func = Func
  { fName :: String
  , fGenerics :: [String]
  , fParams :: [(String,String)]
  , fRet :: Maybe String
  , fBody :: [Stmt] 
  } deriving (Show, Eq)

data Struct = Struct
  { sName :: String
  , sFields :: [(String,String)]
  } deriving (Show, Eq)

parseSL :: [SLToken] -> Either String [Definition]
parseSL toks = case parseSLMP toks of
  Left err -> Left (errorBundlePretty err)
  Right fs -> Right fs

updateParserPosition :: SourcePos -> Parser ()
updateParserPosition pos = updateParserState $ \s -> s { statePosState = (statePosState s) { pstateSourcePos = pos } }

satisfyTok :: (SLToken -> Bool) -> Parser SLToken
satisfyTok f = do
  t <- token testLabel Set.empty
  updateParserPosition (getTokenPos t)
  return t
  where
    testLabel t = if f t then Just t else Nothing

-- | Consome um token específico e retorna um valor extraído dele
matchTok :: (SLToken -> Maybe a) -> Parser a
matchTok f = do
  res <- token (\t -> f t >>= \x -> Just (x, getTokenPos t)) Set.empty
  let (x, pos) = res
  updateParserPosition pos
  return x

isTk :: (SLToken -> Maybe b) -> (SLToken -> Bool)
isTk f t = case f t of { Just _ -> True; Nothing -> False }

isTkPlusPlus, isTkMinusMinus, isTkStar, isTkSlash, isTkPlus, isTkMinus :: SLToken -> Bool
isTkPlusPlus t = case t of { TkPlusPlus _ -> True; _ -> False }
isTkMinusMinus t = case t of { TkMinusMinus _ -> True; _ -> False }
isTkStar t = case t of { TkStar _ -> True; _ -> False }
isTkSlash t = case t of { TkSlash _ -> True; _ -> False }
isTkPlus t = case t of { TkPlus _ -> True; _ -> False }
isTkMinus t = case t of { TkMinus _ -> True; _ -> False }

isTkLe, isTkGe, isTkLt, isTkGt, isTkEqEq, isTkNeq :: SLToken -> Bool
isTkLe t = case t of { TkLe _ -> True; _ -> False }
isTkGe t = case t of { TkGe _ -> True; _ -> False }
isTkLt t = case t of { TkLt _ -> True; _ -> False }
isTkGt t = case t of { TkGt _ -> True; _ -> False }
isTkEqEq t = case t of { TkEqEq _ -> True; _ -> False }
isTkNeq t = case t of { TkNeq _ -> True; _ -> False }

isTkAnd, isTkOr, isTkAssign :: SLToken -> Bool
isTkAnd t = case t of { TkAnd _ -> True; _ -> False }
isTkOr t = case t of { TkOr _ -> True; _ -> False }
isTkAssign t = case t of { TkAssign _ -> True; _ -> False }



pIdent :: Parser String
pIdent = matchTok $ \t -> case t of { TkIdent s _ -> Just s; _ -> Nothing }

pInt :: Parser Integer
pInt = matchTok $ \t -> case t of { TkIntLit i _ -> Just i; _ -> Nothing }

pFloat :: Parser Double
pFloat = matchTok $ \t -> case t of { TkFloatLit f _ -> Just f; _ -> Nothing }

pString :: Parser String
pString = matchTok $ \t -> case t of { TkStringLit s _ -> Just s; _ -> Nothing }



-- Keywords
kwFunc, kwReturn, kwLet, kwIf, kwElse, kwWhile, kwFor, kwStruct, kwNew, kwForall :: Parser SLToken
kwFunc    = matchTok $ \t -> case t of { TkFunc p -> Just t; _ -> Nothing }
kwReturn  = matchTok $ \t -> case t of { TkReturn p -> Just t; _ -> Nothing }
kwLet     = matchTok $ \t -> case t of { TkLet p -> Just t; _ -> Nothing }
kwIf      = matchTok $ \t -> case t of { TkIf p -> Just t; _ -> Nothing }
kwElse    = matchTok $ \t -> case t of { TkElse p -> Just t; _ -> Nothing }
kwWhile   = matchTok $ \t -> case t of { TkWhile p -> Just t; _ -> Nothing }
kwFor     = matchTok $ \t -> case t of { TkFor p -> Just t; _ -> Nothing }
kwStruct  = matchTok $ \t -> case t of { TkStruct p -> Just t; _ -> Nothing }
kwNew     = matchTok $ \t -> case t of { TkNew p -> Just t; _ -> Nothing }
kwForall  = matchTok $ \t -> case t of { TkForall p -> Just t; _ -> Nothing }

-- Symbols
symLParen, symRParen, symLBrace, symRBrace, symLBracket, symRBracket :: Parser SLToken
symComma, symSemicolon, symColon, symDot, symAssign, symArrow, symDoubleColon, symBackSlash :: Parser SLToken

symLParen    = matchTok $ \t -> case t of { TkLParen p -> Just t; _ -> Nothing }
symRParen    = matchTok $ \t -> case t of { TkRParen p -> Just t; _ -> Nothing }
symLBrace    = matchTok $ \t -> case t of { TkLBrace p -> Just t; _ -> Nothing }
symRBrace    = matchTok $ \t -> case t of { TkRBrace p -> Just t; _ -> Nothing }
symLBracket  = matchTok $ \t -> case t of { TkLBracket p -> Just t; _ -> Nothing }
symRBracket  = matchTok $ \t -> case t of { TkRBracket p -> Just t; _ -> Nothing }
symComma     = matchTok $ \t -> case t of { TkComma p -> Just t; _ -> Nothing }
symSemicolon = matchTok $ \t -> case t of { TkSemicolon p -> Just t; _ -> Nothing }
symColon     = matchTok $ \t -> case t of { TkColon p -> Just t; _ -> Nothing }
symDot       = matchTok $ \t -> case t of { TkDot p -> Just t; _ -> Nothing }
symAssign    = matchTok $ \t -> case t of { TkAssign p -> Just t; _ -> Nothing }
symArrow     = matchTok $ \t -> case t of { TkArrow p -> Just t; _ -> Nothing }
symBackSlash = matchTok $ \t -> case t of { TkBackSlash p -> Just t; _ -> Nothing }
symDoubleColon = matchTok $ \t -> case t of { TkDoubleColon p -> Just t; _ -> Nothing }

pFunc :: Parser Func
pFunc = do
    gens   <- try pGenerics <|> return []
    _      <- kwFunc
    name   <- pIdent
    _      <- symLParen
    params <- pParam `sepBy` symComma
    _      <- symRParen
    ret    <- optional (symColon *> pType)
    _      <- symLBrace
    body   <- many pStmt
    _      <- symRBrace
    return (Func name gens params ret body)

pLambda :: Parser Func
pLambda = do
    let name = "lambda"
    _      <- symBackSlash
    gens   <- try pGenerics <|> return []
    params <- pLambdaParams
    ret    <- optional (symDoubleColon *> pType)
    _      <- symArrow
    body <- (symLBrace *> many pStmt <* symRBrace) <|> fmap (\e -> [SExpr e]) pExpr
    return (Func name gens params ret body)

pLambdaParams :: Parser [(String,String)]
pLambdaParams = parenParam <|> singleParam
  where
    parenParam = do
        _ <- symLParen
        params <- pParam `sepBy` symComma
        _ <- symRParen
        return params
    singleParam = do
        p <- pParam
        return [p]


pParam :: Parser (String, String)
pParam = do
    name <- pIdent
    tp   <- optional (symColon *> pType)
    return (name, maybe "" id tp)

pGenerics :: Parser [String]
pGenerics = do
    _      <- kwForall
    idents <- some pIdent
    _      <- symDot
    return idents


pType :: Parser String
pType = do
    base <- pBaseType
    pArraySuffix base

pBaseType :: Parser String
pBaseType = pFuncType <|> pPrimitive <|> pIdent
  where
    pPrimitive = matchTok $ \t -> case t of
        TkIntType _    -> Just "int"
        TkFloatType _  -> Just "float"
        TkStringType _ -> Just "string"
        TkBoolType _   -> Just "bool"
        TkVoid _       -> Just "void"
        _              -> Nothing

pFuncType :: Parser String
pFuncType = do
    _    <- symLParen
    args <- pBaseType `sepBy` symComma
    _    <- symRParen
    ret  <- optional (symArrow *> pType)
    let argStr = concat [a ++ "," | a <- args]
    let retStr = maybe "" (" -> " ++) ret
    return $ "(" ++ argStr ++ ")" ++ retStr

pArraySuffix :: String -> Parser String
pArraySuffix base =
    try (symLBracket *> symRBracket *> pArraySuffix (base ++ "[]"))
    <|> try (do
         _  <- symLBracket
         sz <- pInt
         _  <- symRBracket
         pArraySuffix (base ++ "[" ++ show sz ++ "]"))
    <|> return base


pStmt :: Parser Stmt
pStmt = choice
    [ pReturn
    , pLet
    , pIf
    , pWhile
    , pFor
    , SExpr <$> (pExpr <* optional symSemicolon)
    ]

pReturn :: Parser Stmt
pReturn = SReturn <$> (kwReturn *> pExpr <* symSemicolon)

pLet :: Parser Stmt
pLet = do
    _    <- kwLet
    name <- pIdent
    tp   <- optional (symColon *> pType)
    val  <- optional (symAssign *> pExpr)
    _    <- symSemicolon
    return $ SLet name tp val

pGlobalLet :: Parser GlobalLet
pGlobalLet = do
    _    <- kwLet
    name <- pIdent
    tp   <- optional (symColon *> pType)
    val  <- optional (symAssign *> pExpr)
    _    <- symSemicolon
    return $ GlobalLet name tp val

pIf :: Parser Stmt
pIf = do
    _        <- kwIf
    _        <- symLParen
    cond     <- pExpr
    _        <- symRParen
    _        <- symLBrace
    thenBody <- many pStmt
    _        <- symRBrace
    elseBody <- optional (kwElse *> symLBrace *> many pStmt <* symRBrace)
    return $ SIf cond thenBody elseBody

pWhile :: Parser Stmt
pWhile = do
    _    <- kwWhile
    _    <- symLParen
    cond <- pExpr
    _    <- symRParen
    _    <- symLBrace
    body <- many pStmt
    _    <- symRBrace
    return $ SWhile cond body

pFor :: Parser Stmt
pFor = do
    _    <- kwFor -- serve para consumir a palavra-chave 'for'
    _    <- symLParen -- serve para consumir o '('
    init <- optional $ choice
            [ try pLet
            , SExpr <$> pExpr <* symSemicolon
            , symSemicolon *> return (SExpr (EInt 0)) -- dummy
            ]
    
    cond <- pExpr
    _    <- symSemicolon
    incr <- optional pExpr
    _    <- symRParen
    _    <- symLBrace
    body <- many pStmt
    _    <- symRBrace
    return $ SFor init cond incr body


pExpr :: Parser Expr
pExpr = makeExprParser pTerm table

table :: [[Operator Parser Expr]]
table =
  [ [ prefix isTkPlusPlus "++", prefix isTkMinusMinus "--" ]
  , [ InfixL (EBin "*" <$ op isTkStar)
    , InfixL (EBin "/" <$ op isTkSlash) ]
  , [ InfixL (EBin "+" <$ op isTkPlus)
    , InfixL (EBin "-" <$ op isTkMinus) ]
  , [ InfixL (EBin "<=" <$ op isTkLe)
    , InfixL (EBin ">=" <$ op isTkGe)
    , InfixL (EBin "<"  <$ op isTkLt)
    , InfixL (EBin ">"  <$ op isTkGt) ]
  , [ InfixL (EBin "==" <$ op isTkEqEq)
    , InfixL (EBin "!=" <$ op isTkNeq) ]
  , [ InfixL (EBin "&&" <$ op isTkAnd) ]
  , [ InfixL (EBin "||" <$ op isTkOr) ]
  , [ InfixR (EBin "="  <$ op isTkAssign) ]
  ]
  where
    -- consume a token matching predicate
    op :: (SLToken -> Bool) -> Parser ()
    op p = void $ satisfyTok p
    prefix :: (SLToken -> Bool) -> String -> Operator Parser Expr
    prefix pred str = Prefix (EPre str <$ op pred)


pTerm :: Parser Expr
pTerm = pPostfix atom
  where
    atom = choice
      [ EInt    <$> pInt
      , EFloat  <$> pFloat
      , EString <$> pString
      , pNew
      , pArrayLit
      , try (symLParen *> pExpr <* symRParen)
      , ELambda <$> pLambda
      , EVar    <$> pIdent
      ]

pNew :: Parser Expr
pNew = do
    _  <- kwNew
    tp <- pBaseType
    _  <- symLBracket
    sz <- pExpr
    _  <- symRBracket
    return $ ENew tp sz

pArrayLit :: Parser Expr
pArrayLit = do
    _  <- symLBracket
    es <- pExpr `sepBy` symComma
    _  <- symRBracket
    return $ EArray es

pPostfix :: Parser Expr -> Parser Expr
pPostfix p = do
    x <- p
    postfixes x
  where
  postfixes x = choice
    [ do
      _  <- symLBracket
      ix <- pExpr
      _  <- symRBracket
      postfixes (EBin "[]" x ix)
    , do
      _   <- symDot
      fld <- pIdent
      postfixes (EBin "." x (EVar fld))
    , try (do
      -- struct/record literal: Ident { e1, e2, ... }
      _ <- satisfyTok $ \t -> case t of { TkLBrace _ -> True; _ -> False }
      -- parse values inside braces
      vals <- pExpr `sepBy` symComma
      _ <- symRBrace
      let name = case x of { EVar n -> n; _ -> "" }
      postfixes (ECall name vals))
    , try (do
      args <- symLParen *> (pExpr `sepBy` symComma) <* symRParen
      let name = case x of { EVar n -> n; _ -> "expr_call" }
      postfixes (ECall name args))
    , do
      _ <- void $ satisfyTok isTkPlusPlus
      postfixes (EPost x "++")
    , do
      _ <- void $ satisfyTok isTkMinusMinus
      postfixes (EPost x "--")
    , return x
    ]

