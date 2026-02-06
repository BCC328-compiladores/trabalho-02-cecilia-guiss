module ASTtoTree where

import Data.Tree
import Parser (Definition(..), Func(..), Struct(..), Stmt(..), Expr(..))

-- Converte lista de Definições para Tree
defsToTree :: [Definition] -> Tree String
defsToTree defs = Node "Program" (map defToTree defs)

defToTree :: Definition -> Tree String
defToTree (DefFunc f) = funcToTree f
defToTree (DefStruct s) = structToTree s

structToTree :: Struct -> Tree String
structToTree (Struct name fields) =
    Node ("Struct: " ++ name) (map fieldToTree fields)
  where
    fieldToTree (n, t) = Node ("Field: " ++ n ++ " : " ++ t) []

funcToTree :: Func -> Tree String
funcToTree (Func name generics params ret body) = 
    Node ("Func: " ++ name ++ " " ++ showGenerics generics ++ "(" ++ showParams params ++ ") -> " ++ showRet ret) 
         [ Node "Body" (map stmtToTree body) ]
  where
    showGenerics [] = ""
    showGenerics gs = "forall " ++ unwords gs ++ " . "
    showParams ps = unwords [n ++ ":" ++ t | (n, t) <- ps]
    showRet Nothing = "void"
    showRet (Just t) = t

stmtToTree :: Stmt -> Tree String
stmtToTree (SReturn e) = Node "Return" [exprToTree e]
stmtToTree (SLet name typeStr mExpr) = 
    Node ("Let " ++ name ++ maybe "" (": " ++) typeStr) (maybe [] (\e -> [exprToTree e]) mExpr)
stmtToTree (SIf cond thenBlk mElseBlk) = 
    Node "If" ([exprToTree cond, Node "Then" (map stmtToTree thenBlk)] ++ 
               maybe [] (\blk -> [Node "Else" (map stmtToTree blk)]) mElseBlk)
stmtToTree (SWhile cond body) = 
    Node "While" [exprToTree cond, Node "Body" (map stmtToTree body)]
stmtToTree (SFor mInit cond mIncr body) = 
    Node "For" [ Node "Init" (maybe [] (\s -> [stmtToTree s]) mInit)
               , Node "Cond" [exprToTree cond]
               , Node "Incr" (maybe [] (\e -> [exprToTree e]) mIncr)
               , Node "Body" (map stmtToTree body)
               ]
stmtToTree (SExpr e) = Node "StmtExpr" [exprToTree e]

exprToTree :: Expr -> Tree String
exprToTree (EInt i) = Node ("Int: " ++ show i) []
exprToTree (EFloat f) = Node ("Float: " ++ show f) []
exprToTree (EString s) = Node ("String: " ++ show s) []
exprToTree (EVar s) = Node ("Var: " ++ s) []
exprToTree (ECall name args) = Node ("Call: " ++ name) (map exprToTree args)
exprToTree (EArray elems) = Node "ArrayLit" (map exprToTree elems)
exprToTree (ENew ty size) = Node ("New " ++ ty) [exprToTree size]
exprToTree (EBin op e1 e2) = Node ("BinOp: " ++ op) [exprToTree e1, exprToTree e2]
exprToTree (EPost e op) = Node ("Postfix: " ++ op) [exprToTree e]
exprToTree (EPre op e) = Node ("Prefix: " ++ op) [exprToTree e]
