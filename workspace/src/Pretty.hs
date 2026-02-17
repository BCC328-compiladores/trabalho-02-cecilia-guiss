module Pretty where

import Prettyprinter
import Text.Megaparsec.Pos (SourcePos, unPos, sourceLine, sourceColumn, sourcePosPretty)
import SLToken
import Parser (Definition(..), Func(..), Struct(..), Stmt(..), Expr(..), GlobalLet(..))

-- ... existing code ...


prettyLineCol :: SourcePos -> Doc ann
prettyLineCol pos =
  pretty (unPos (sourceLine pos)) <> pretty ":" <> pretty (unPos (sourceColumn pos))

prettyToken :: SLToken -> Doc ann
prettyToken tok = case tok of
  TkIdent s pos      -> pretty "Ident(" <> pretty s <> pretty ")" <+> prettyLineCol pos
  TkIntLit n pos     -> pretty "Int(" <> pretty n <> pretty ")" <+> prettyLineCol pos
  TkFloatLit f pos   -> pretty "Float(" <> pretty f <> pretty ")" <+> prettyLineCol pos
  TkStringLit s pos  -> pretty "String(" <> pretty s <> pretty ")" <+> prettyLineCol pos
  TkBoolLit b pos    -> pretty "Bool(" <> pretty b <> pretty ")" <+> prettyLineCol pos

  -- Palavras reservadas
  TkFunc pos         -> pretty "func" <+> prettyLineCol pos
  TkLet pos          -> pretty "let" <+> prettyLineCol pos
  TkReturn pos       -> pretty "return" <+> prettyLineCol pos
  TkIf pos           -> pretty "if" <+> prettyLineCol pos
  TkElse pos         -> pretty "else" <+> prettyLineCol pos
  TkWhile pos        -> pretty "while" <+> prettyLineCol pos
  TkFor pos          -> pretty "for" <+> prettyLineCol pos
  TkStruct pos       -> pretty "struct" <+> prettyLineCol pos
  TkNew pos          -> pretty "new" <+> prettyLineCol pos
  TkForall pos       -> pretty "forall" <+> prettyLineCol pos
  TkVoid pos         -> pretty "void" <+> prettyLineCol pos

  -- Tipos
  TkIntType pos      -> pretty "IntType" <+> prettyLineCol pos
  TkFloatType pos    -> pretty "FloatType" <+> prettyLineCol pos
  TkStringType pos   -> pretty "StringType" <+> prettyLineCol pos
  TkBoolType pos     -> pretty "BoolType" <+> prettyLineCol pos

  -- Símbolos
  TkLParen pos       -> pretty "(" <+> prettyLineCol pos
  TkRParen pos       -> pretty ")" <+> prettyLineCol pos
  TkLBrace pos       -> pretty "{" <+> prettyLineCol pos
  TkRBrace pos       -> pretty "}" <+> prettyLineCol pos
  TkLBracket pos     -> pretty "[" <+> prettyLineCol pos
  TkRBracket pos     -> pretty "]" <+> prettyLineCol pos
  TkComma pos        -> pretty "," <+> prettyLineCol pos
  TkSemicolon pos    -> pretty ";" <+> prettyLineCol pos
  TkColon pos        -> pretty ":" <+> prettyLineCol pos
  TkDot pos          -> pretty "." <+> prettyLineCol pos
  TkAssign pos       -> pretty "=" <+> prettyLineCol pos
  TkArrow pos        -> pretty "->" <+> prettyLineCol pos
  TkBackSlash pos    -> pretty "\\" <+> prettyLineCol pos
  TkDoubleColon pos  -> pretty "::" <+> prettyLineCol pos

  -- Operadores
  TkPlus pos         -> pretty "+" <+> prettyLineCol pos
  TkPlusPlus pos     -> pretty "++" <+> prettyLineCol pos
  TkMinusMinus pos   -> pretty "--" <+> prettyLineCol pos
  TkMinus pos        -> pretty "-" <+> prettyLineCol pos
  TkStar pos         -> pretty "*" <+> prettyLineCol pos
  TkSlash pos        -> pretty "/" <+> prettyLineCol pos
  TkLt pos           -> pretty "<" <+> prettyLineCol pos
  TkGt pos           -> pretty ">" <+> prettyLineCol pos
  TkLe pos           -> pretty "<=" <+> prettyLineCol pos
  TkGe pos           -> pretty ">=" <+> prettyLineCol pos
  TkEqEq pos         -> pretty "==" <+> prettyLineCol pos
  TkNeq pos          -> pretty "!=" <+> prettyLineCol pos
  TkAnd pos          -> pretty "&&" <+> prettyLineCol pos
  TkOr pos           -> pretty "||" <+> prettyLineCol pos
  TkNot pos          -> pretty "!" <+> prettyLineCol pos



-- Gerar código com prettyprinter
prettyDefinition (DefFunc f) = prettyFunc f
prettyDefinition (DefStruct s) = prettyStruct s
prettyDefinition (DefGlobalLet g) = prettyGlobalLet g

prettyGlobalLet :: GlobalLet -> Doc ann
prettyGlobalLet (GlobalLet name mType mExpr) = 
    pretty "let" <+> pretty name <> showType mType <+> showInit mExpr <> semi
  where
    showType Nothing = mempty
    showType (Just t) = pretty ":" <+> pretty t
    showInit Nothing = mempty
    showInit (Just e) = space <> equals <+> prettyExpr e

prettyStruct :: Struct -> Doc ann
prettyStruct (Struct name fields) =
    pretty "struct" <+> pretty name <+> lbrace <> line <>
    indent 4 (vsep (map prettyField fields)) <> line <>
    rbrace
  where
    prettyField (n, t) = pretty n <+> colon <+> pretty t <> semi

prettyFunc :: Func -> Doc ann
prettyFunc (Func name generics params ret body) =
    showGenerics generics <> pretty "func" <+> pretty name <+> parens (hsep $ punctuate comma (map prettyParam params)) <> showRet ret <+> lbrace <> line <>
    indent 4 (vsep (map prettyStmt body)) <> line <>
    rbrace
  where
    showGenerics [] = mempty
    showGenerics gs = pretty "forall" <+> hsep (map pretty gs) <+> dot <> space
    
    showRet Nothing = space <> pretty ":" <+> pretty "void"
    showRet (Just t) = space <> pretty ":" <+> pretty t

prettyParam :: (String, String) -> Doc ann
prettyParam (n, t) = pretty n <+> pretty ":" <+> pretty t

prettyStmt :: Stmt -> Doc ann
prettyStmt stmt = case stmt of
    SReturn e -> pretty "return" <+> prettyExpr e <> semi
    SLet name mType mExpr -> 
        pretty "let" <+> pretty name <> showType mType <+> showInit mExpr <> semi
    SIf cond thenBody mElse -> 
        pretty "if" <+> parens (prettyExpr cond) <+> lbrace <> line <>
        indent 4 (vsep (map prettyStmt thenBody)) <> line <>
        rbrace <> showElse mElse
    SWhile cond body ->
        pretty "while" <+> parens (prettyExpr cond) <+> lbrace <> line <>
        indent 4 (vsep (map prettyStmt body)) <> line <>
        rbrace
    SFor mInit cond mIncr body ->
        pretty "for" <+> parens (
            maybe mempty prettyStmtForInit mInit <+>
            prettyExpr cond <> semi <+>
            maybe mempty prettyExpr mIncr
        ) <+> lbrace <> line <>
        indent 4 (vsep (map prettyStmt body)) <> line <>
        rbrace
    SExpr e -> prettyExpr e <> semi

    where
      showType Nothing = mempty
      showType (Just t) = pretty ":" <+> pretty t
      showInit Nothing = mempty
      showInit (Just e) = space <> equals <+> prettyExpr e
      
      showElse Nothing = mempty
      showElse (Just b) = space <> pretty "else" <+> lbrace <> line <> indent 4 (vsep (map prettyStmt b)) <> line <> rbrace

     
      prettyStmtForInit (SLet n t e) = pretty "let" <+> pretty n <> showType t <+> showInit e <> semi
      prettyStmtForInit (SExpr e) = prettyExpr e <> semi
      prettyStmtForInit _ = mempty 

prettyExpr :: Expr -> Doc ann
prettyExpr expr = case expr of
    EInt n -> pretty n
    EFloat f -> pretty f
    EString s -> dquotes (pretty s)
    EVar s -> pretty s
    ECall name args -> pretty name <> parens (hsep $ punctuate comma (map prettyExpr args))
    EArray elems -> brackets (hsep $ punctuate comma (map prettyExpr elems))
    ENew t e -> pretty "new" <+> pretty t <> brackets (prettyExpr e)
    EBin "[]" e1 e2 -> prettyExpr e1 <> brackets (prettyExpr e2)
    EBin op e1 e2 -> prettyExpr e1 <+> pretty op <+> prettyExpr e2
    EPre op e -> pretty op <> prettyExpr e
    EPost e op -> prettyExpr e <> pretty op
    ELambda f -> prettyFunc f


-- Converter AST para Pretty