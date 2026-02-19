module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (isSuffixOf, isInfixOf, isPrefixOf, intercalate)
import Parser
import Types

-- Valores em tempo de execução
data Value
    = VInt Int
    | VFloat Double
    | VBool Bool
    | VString String
    | VArray [Value]
    | VStruct (Map String Value)
    | VVoid
    | VFunc [String] [Stmt] (Map String Value) -- Closure simples (params, corpo, contexto capturado)
    | VNull
    | VClosure Value Value -- (Função, Ambiente)
    deriving (Eq)

instance Show Value where
    show (VInt i) = show i
    show (VFloat f) = show f
    show (VBool b) = if b then "true" else "false"
    show (VString s) = s
    show (VArray arr) = "[" ++ (intercalate ", " (map show arr)) ++ "]"
    show (VStruct m) = "struct { " ++ (intercalate ", " [k ++ ": " ++ show v | (k, v) <- Map.toList m]) ++ " }"
    show VVoid = "void"
    show (VFunc _ _ _) = "<function>"
    show VNull = "null"
    show (VClosure _ _) = "<closure>"

-- Estado do Interpretador
data InterpState = InterpState
    { scopes  :: [Map String Value] -- Pilha de escopos (o topo é o atual)
    , globals :: Map String Value
    , structs :: Map String [String]
    }

type InterpM a = ExceptT String (StateT InterpState IO) a

runInterpreter :: [Definition] -> IO (Either String ())
runInterpreter defs = do
    let initialState = InterpState [Map.empty] Map.empty Map.empty
    evalStateT (runExceptT (interpretProgram defs)) initialState

interpretProgram :: [Definition] -> InterpM ()
interpretProgram defs = do
    -- Coleta definições globais
    mapM_ collectDef defs
    -- Procura pela função main
    st <- get
    case Map.lookup "main" (globals st) of
        Just (VFunc _ body _) -> do
            void $ execBlock body
        _ -> throwError "Função 'main' não encontrada"

collectDef :: Definition -> InterpM ()
collectDef (DefFunc (Func name _ params _ body)) = do
    let paramNames = map fst params
    modify $ \s -> s { globals = Map.insert name (VFunc paramNames body Map.empty) (globals s) }
collectDef (DefStruct (Struct name fields)) = do
    let fieldNames = map fst fields
    modify $ \s -> s { structs = Map.insert name fieldNames (structs s) }

-- Execução de blocos e comandos
execBlock :: [Stmt] -> InterpM Value
execBlock stmts = do
    enterScope
    res <- execStmts stmts
    exitScope
    return res

execStmts :: [Stmt] -> InterpM Value
execStmts [] = return VVoid
execStmts (s:ss) = do
    res <- execStmt s
    case res of
        VVoid -> execStmts ss
        other -> return other -- Se for um Return, propaga o valor

enterScope :: InterpM ()
enterScope = modify $ \s -> s { scopes = Map.empty : scopes s }

exitScope :: InterpM ()
exitScope = do
    st <- get
    case scopes st of
        (_:rest) -> put st { scopes = rest }
        [] -> throwError "Erro interno: Fim de pilha de escopo"

lookupVar :: String -> InterpM Value
lookupVar name = do
    st <- get
    case findInScopes name (scopes st) of
        Just v -> return v
        Nothing -> case Map.lookup name (globals st) of
            Just v -> return v
            Nothing -> throwError $ "Variável não encontrada: " ++ name

findInScopes :: String -> [Map String Value] -> Maybe Value
findInScopes _ [] = Nothing
findInScopes name (s:ss) = case Map.lookup name s of
    Just v -> Just v
    Nothing -> findInScopes name ss

execStmt :: Stmt -> InterpM Value
execStmt (SReturn e) = evalExpr e

execStmt (SLet name (Just tStr) Nothing) 
    | "[" `isInfixOf` tStr && "]" `isSuffixOf` tStr = do
        let sizePart = takeWhile (/= ']') (tail (dropWhile (/= '[') tStr))
        if not (null sizePart) && all (`elem` "0123456789") sizePart
            then do
                let n = read sizePart
                defineVar name (VArray (replicate n VNull))
                return VVoid
            else do
                defineVar name VNull
                return VVoid

execStmt (SLet name _ (Just e)) = do
    val <- evalExpr e
    defineVar name val
    return VVoid

execStmt (SLet name _ Nothing) = do
    defineVar name VNull
    return VVoid

execStmt (SIf cond thenBlk mElseBlk) = do
    cv <- evalExpr cond
    case cv of
        VBool True -> execBlock thenBlk
        VBool False -> case mElseBlk of
            Just elseBlk -> execBlock elseBlk
            Nothing -> return VVoid
        _ -> throwError "Condição do 'if' deve ser booleana"

execStmt (SWhile cond body) = do
    cv <- evalExpr cond
    case cv of
        VBool True -> do
            res <- execBlock body
            case res of
                VVoid -> execStmt (SWhile cond body)
                other -> return other
        VBool False -> return VVoid
        _ -> throwError "Condição do 'while' deve ser booleana"

execStmt (SFor mInit cond mIncr body) = do
    enterScope
    case mInit of
        Just (SExpr (EBin "=" (EVar name) valExpr)) -> do
            val <- evalExpr valExpr
            st <- get
            case findInScopes name (scopes st) of
                Just _ -> updateAssign (EVar name) val
                Nothing -> defineVar name val
        Just initS -> void $ execStmt initS
        Nothing -> return ()
    
    let loop = do
            cv <- evalExpr cond
            case cv of
                VBool True -> do
                    res <- execBlock body
                    case res of
                        VVoid -> do
                            case mIncr of
                                Just incrE -> void $ evalExpr incrE
                                Nothing -> return ()
                            loop
                        other -> return other
                VBool False -> return VVoid
                _ -> throwError "Condição do 'for' deve ser booleana"
    
    res <- loop
    exitScope
    return res

execStmt (SExpr e) = void (evalExpr e) >> return VVoid

defineVar :: String -> Value -> InterpM ()
defineVar name val = modify $ \s -> s { scopes = Map.insert name val (head (scopes s)) : tail (scopes s) }

-- Avaliação de expressões
evalExpr :: Expr -> InterpM Value
evalExpr (EInt i) = return (VInt (fromIntegral i))
evalExpr (EFloat f) = return (VFloat f)
evalExpr (EString s) = return (VString s)
evalExpr (EVar n) = lookupVar n

evalExpr (EBin "=" lhs e2) = do
    val <- evalExpr e2
    updateAssign lhs val
    return val

evalExpr (EBin op e1 e2)
    | op == "&&" = do
        v1 <- evalExpr e1
        case v1 of
            VBool False -> return (VBool False)
            VBool True -> evalExpr e2
            _ -> throwError "Operador && requer booleanos"
    | op == "||" = do
        v1 <- evalExpr e1
        case v1 of
            VBool True -> return (VBool True)
            VBool False -> evalExpr e2
            _ -> throwError "Operador || requer booleanos"
    | op == "." = do
        v1 <- evalExpr e1
        case (v1, e2) of
            (VArray arr, EVar "size") -> return (VInt (length arr))
            (VStruct fields, EVar fName) -> 
                case Map.lookup fName fields of
                    Just v -> return v
                    Nothing -> throwError $ "Campo " ++ fName ++ " não encontrado"
            _ -> throwError "Acesso a campo inválido"

evalExpr (EBin op e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    applyBinOp op v1 v2

evalExpr (ECall "print" args) = do
    vals <- mapM evalExpr args
    liftIO $ putStrLn (concatMap show vals)
    return VVoid

evalExpr (ECall "make_closure" [f, env]) = do
    fv <- evalExpr f
    ev <- evalExpr env
    return (VClosure fv ev)

evalExpr (ECall name args)
    | "new_" `isPrefixOf` name = do
        let sName = drop 4 name
        st <- get
        case Map.lookup sName (structs st) of
            Just fieldNames -> do
                argVals <- mapM evalExpr args
                return $ VStruct (Map.fromList (zip fieldNames argVals))
            Nothing -> throwError $ "Struct " ++ sName ++ " não encontrada"
    | otherwise = do
    st <- get
    case Map.lookup name (structs st) of
        Just fieldNames -> do
            argVals <- mapM evalExpr args
            if length fieldNames /= length argVals
                then throwError $ "Número incorreto de argumentos para struct " ++ name
                else return $ VStruct (Map.fromList (zip fieldNames argVals))
        Nothing -> do
            fVal <- lookupVar name
            case fVal of
                VFunc params body closureEnv -> do
                    argVals <- mapM evalExpr args
                    applyFunc params body (Map.union (Map.fromList (zip params argVals)) closureEnv)
                VClosure (VFunc params body closureEnv) envVal -> do
                    argVals <- mapM evalExpr args
                    -- Passa o ambiente como primeiro argumento e o resto depois
                    let allArgs = envVal : argVals 
                    applyFunc params body (Map.union (Map.fromList (zip params allArgs)) closureEnv)
                _ -> throwError $ name ++ " não é uma função ou closure"


evalExpr (EArray elems) = do
    vals <- mapM evalExpr elems
    return (VArray vals)

evalExpr (ENew _ sizeE) = do
    sv <- evalExpr sizeE
    case sv of
        VInt n -> return (VArray (replicate n VNull))
        _ -> throwError "Tamanho do array deve ser inteiro"

evalExpr (EPost (EVar n) op) = do
    v <- evalExpr (EVar n)
    case v of
        VInt i -> do
            let nv = if op == "++" then VInt (i+1) else VInt (i-1)
            updateAssign (EVar n) nv
            return v
        _ -> throwError "Operador ++/-- requer inteiro"

evalExpr (EPre op (EVar n)) = do
    v <- evalExpr (EVar n)
    case v of
        VInt i -> do
            let nv = if op == "++" then VInt (i+1) else VInt (i-1)
            updateAssign (EVar n) nv
            return nv
        _ -> throwError "Operador ++/-- requer inteiro"

evalExpr _ = throwError "Expressão não suportada no interpretador"

applyFunc :: [String] -> [Stmt] -> Map String Value -> InterpM Value
applyFunc params body env = do
    oldScopes <- gets scopes
    modify $ \s -> s { scopes = [env] }
    res <- execStmts body
    modify $ \s -> s { scopes = oldScopes }
    return res

-- Função auxiliar para lidar com atribuições 
updateAssign :: Expr -> Value -> InterpM ()
updateAssign (EVar name) val = do
    st <- get
    case updateInScopes name val (scopes st) of
        Just newScopes -> modify $ \s -> s { scopes = newScopes }
        Nothing -> if Map.member name (globals st)
            then modify $ \s -> s { globals = Map.insert name val (globals s) }
            else throwError $ "Variável não declarada: " ++ name

updateAssign (EBin "[]" eArr eIdx) val = do
    arrVal <- evalExpr eArr
    idxVal <- evalExpr eIdx
    case (arrVal, idxVal) of
        (VArray arr, VInt i) -> do
            if i >= 0 && i < length arr then do
                let (before, _:after) = splitAt i arr
                let newArr = VArray (before ++ val : after)
                updateAssign eArr newArr
            else throwError "Índice de array fora de limites"
        _ -> throwError "LHS de atribuição de índice inválido"
updateAssign (EBin "." eStruct (EVar fName)) val = do
    sVal <- evalExpr eStruct
    case sVal of
        VStruct fields -> do
            let newFields = Map.insert fName val fields
            updateAssign eStruct (VStruct newFields)
        _ -> throwError "LHS de atribuição de campo inválido"
updateAssign _ _ = throwError "Alvo de atribuição (L-Value) inválido"

updateInScopes :: String -> Value -> [Map String Value] -> Maybe [Map String Value]
updateInScopes _ _ [] = Nothing
updateInScopes name val (s:ss) = 
    if Map.member name s
        then Just (Map.insert name val s : ss)
        else fmap (s:) (updateInScopes name val ss)


-- Operações binárias, foi implementado para que não seja possível operações entre 2 tipos númericos diferentes, ex: int + float
applyBinOp :: String -> Value -> Value -> InterpM Value
applyBinOp "+" (VInt a) (VInt b) = return (VInt (a + b))
applyBinOp "+" (VFloat a) (VFloat b) = return (VFloat (a + b))
applyBinOp "-" (VInt a) (VInt b) = return (VInt (a - b))
applyBinOp "-" (VFloat a) (VFloat b) = return (VFloat (a - b))
applyBinOp "*" (VInt a) (VInt b) = return (VInt (a * b))
applyBinOp "*" (VFloat a) (VFloat b) = return (VFloat (a * b))
applyBinOp "/" (VInt a) (VInt b) = if b == 0 then throwError "Divisão por zero" else return (VInt (a `div` b))
applyBinOp "/" (VFloat a) (VFloat b) = if b == 0.0 then throwError "Divisão por zero" else return (VFloat (a / b))
applyBinOp "<" (VInt a) (VInt b) = return (VBool (a < b))
applyBinOp "<" (VFloat a) (VFloat b) = return (VBool (a < b))
applyBinOp ">" (VInt a) (VInt b) = return (VBool (a > b))
applyBinOp ">" (VFloat a) (VFloat b) = return (VBool (a > b))
applyBinOp "<=" (VInt a) (VInt b) = return (VBool (a <= b))
applyBinOp "<=" (VFloat a) (VFloat b) = return (VBool (a <= b))
applyBinOp ">=" (VInt a) (VInt b) = return (VBool (a >= b))
applyBinOp ">=" (VFloat a) (VFloat b) = return (VBool (a >= b))
applyBinOp "==" v1 v2 = return (VBool (v1 == v2))
applyBinOp "!=" v1 v2 = return (VBool (v1 /= v2))
applyBinOp "[]" (VArray arr) (VInt i) = 
    if i >= 0 && i < length arr then return (arr !! i) else throwError "Índice fora dos limites"
applyBinOp op _ _ = throwError $ "Operador " ++ op ++ " não suportado para estes tipos"
