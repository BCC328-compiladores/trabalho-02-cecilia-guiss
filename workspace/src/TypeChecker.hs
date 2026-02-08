module TypeChecker where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad (void, foldM, zipWithM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Types
import Parser

-- Ambiente do Verificador de Tipos
data Env = Env
    { vars    :: Map String Type                 -- Variáveis locais e parâmetros
    , funcs   :: Map String ([Type], Type)       -- Definições de funções globais
    , structs :: Map String (Map String Type)    -- Layouts de Estruturas
    , context :: [Map String Type]               -- Pilha de escopos
    , retType :: Maybe Type                      -- Tipo de retorno esperado (da função atual)
    }

type CheckM a = ExceptT String (State Env) a

runCheck :: CheckM a -> Either String a
runCheck ma = evalState (runExceptT ma) emptyEnv

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty [] Nothing

-- Verifica se uma variável está no escopo atual
isVarInScope :: String -> CheckM Bool
isVarInScope name = do
    st <- get
    return $ Map.member name (vars st)

enterScope :: CheckM ()
enterScope = do
    st <- get
    put $ st { context = vars st : context st } 

exitScope :: CheckM ()
exitScope = do
    st <- get
    case context st of
        [] -> throwError "Erro interno: Esvaziamento de escopo"
        (oldVars:rest) -> put $ st { vars = oldVars, context = rest }

lookupVar :: String -> CheckM Type
lookupVar name = do
    st <- get
    case Map.lookup name (vars st) of
        Just t -> return t
        Nothing -> throwError $ "Variável não está no escopo: " ++ name

defineVar :: String -> Type -> CheckM ()
defineVar name t = do
    st <- get
    put $ st { vars = Map.insert name t (vars st) }

-- Verifica se o tipo real coincide com o esperado
expectType :: Type -> Type -> CheckM ()
expectType expected actual = 
    if expected == actual 
    then return () 
    else throwError $ "Incompatibilidade de tipos: esperado " ++ show expected ++ ", obtido " ++ show actual

-- Ponto de entrada para checagem do programa
checkProgram :: [Definition] -> Either String ()
checkProgram defs = runCheck $ do
    mapM_ collectSignatures defs
    mapM_ checkDef defs

-- Coleta assinaturas de funções e structs antes de verificar corpos
collectSignatures :: Definition -> CheckM ()
collectSignatures (DefStruct (Struct name fields)) = do
    fieldMap <- foldM (\acc (n, tStr) -> do
        if Map.member n acc
            then throwError $ "Campo duplicado '" ++ n ++ "' na struct '" ++ name ++ "'"
            else return $ Map.insert n (parseTypeStr tStr) acc
        ) Map.empty fields
    
    st <- get
    if Map.member name (structs st)
        then throwError $ "Definição de struct duplicada: " ++ name
        else put $ st { structs = Map.insert name fieldMap (structs st) }

collectSignatures (DefFunc (Func name _ params ret body)) = do
    let paramTypes = map (parseTypeStr . snd) params
    let retTypeVal = maybe TInt parseTypeStr ret -- Padrão int
    st <- get
    if Map.member name (funcs st)
        then throwError $ "Definição de função duplicada: " ++ name
        else put $ st { funcs = Map.insert name (paramTypes, retTypeVal) (funcs st) }

-- Verifica o corpo de uma definição
checkDef :: Definition -> CheckM ()
checkDef (DefStruct _) = return ()
checkDef (DefFunc (Func name _ params ret body)) = do
    let retTypeVal = maybe TInt parseTypeStr ret
    st <- get
    put $ st { retType = Just retTypeVal }
    
    enterScope
    mapM_ (\(n, tStr) -> defineVar n (parseTypeStr tStr)) params
    mapM_ checkStmt body
    exitScope
    
    put $ st { retType = Nothing }

-- Verifica comandos/sentenças
checkStmt :: Stmt -> CheckM ()
checkStmt (SReturn e) = do
    t <- checkExpr e
    st <- get
    case retType st of
        Just rt -> expectType rt t
        Nothing -> throwError "Comando 'return' fora de uma função"

checkStmt (SLet name tStrVal mExpr) = do
    tRaw <- case mExpr of
        Just e -> checkExpr e
        Nothing -> return TVoid 
    
    let tFinal = case tStrVal of
            Just tStr -> parseTypeStr tStr
            Nothing -> tRaw 
            
    case mExpr of
        Just _ -> expectType tFinal tRaw
        Nothing -> return ()

    defineVar name tFinal

checkStmt (SIf cond thenBlk mElseBlk) = do
    tCond <- checkExpr cond
    expectType TBool tCond
    enterScope
    mapM_ checkStmt thenBlk
    exitScope
    case mElseBlk of
        Just elseBlk -> do
            enterScope
            mapM_ checkStmt elseBlk
            exitScope
        Nothing -> return ()

checkStmt (SWhile cond body) = do
    tCond <- checkExpr cond
    expectType TBool tCond
    enterScope
    mapM_ checkStmt body
    exitScope

checkStmt (SFor mInit cond mIncr body) = do
    enterScope
    case mInit of
        Just (SExpr (EBin "=" (EVar name) val)) -> do
            exists <- isVarInScope name
            if exists 
                then void $ checkExpr (EBin "=" (EVar name) val)
                else do
                    tVal <- checkExpr val
                    defineVar name tVal
        Just initStmt -> checkStmt initStmt
        Nothing -> return ()
    
    tCond <- checkExpr cond
    expectType TBool tCond
    
    case mIncr of
        Just incrExpr -> void $ checkExpr incrExpr
        Nothing -> return ()
        
    enterScope
    mapM_ checkStmt body
    exitScope
    exitScope

checkStmt (SExpr e) = void $ checkExpr e

-- Verifica expressões
checkExpr :: Expr -> CheckM Type
checkExpr (EInt _) = return TInt
checkExpr (EFloat _) = return TFloat
checkExpr (EString _) = return TString
checkExpr (EVar name) = lookupVar name

checkExpr (ECall name args)
    | name == "print" = do
        mapM_ checkExpr args
        return TVoid
    | otherwise = do
        st <- get
        -- Verifica se é construtor de struct
        case Map.lookup name (structs st) of
            Just _ -> return (TStruct name) 
            Nothing -> do
                argTypes <- mapM checkExpr args
                -- Verifica funções globais
                case Map.lookup name (funcs st) of
                    Just (expectedArgs, ret) -> do
                        if length expectedArgs /= length argTypes
                            then throwError $ "Número de argumentos incorreto em: " ++ name
                            else zipWithM_ expectType expectedArgs argTypes
                        return ret
                    Nothing -> do
                        -- Verifica variáveis locais (Funções de Ordem Superior)
                        case Map.lookup name (vars st) of
                            Just (TFunc expectedArgs ret) -> do
                                if length expectedArgs /= length argTypes
                                    then throwError $ "Número de argumentos incorreto na chamada HOF: " ++ name
                                    else zipWithM_ expectType expectedArgs argTypes
                                return ret
                            Just _ -> throwError $ "Variável " ++ name ++ " não é uma função"
                            Nothing -> throwError $ "Função não está no escopo: " ++ name

checkExpr (EArray elems) = do
    if null elems
        then return (TArray TVoid)
        else do
            t1 <- checkExpr (head elems)
            mapM_ (\e -> do { t <- checkExpr e; expectType t1 t }) (tail elems)
            return (TArray t1)

checkExpr (ENew tStr size) = do
    tSize <- checkExpr size
    expectType TInt tSize
    return (TArray (parseTypeStr tStr))

checkExpr (EBin op e1 e2)
    | op == "[]" = do
        tArr <- checkExpr e1
        tIdx <- checkExpr e2
        expectType TInt tIdx
        case tArr of
            TArray tElem -> return tElem
            _ -> throwError "Indexação requer um tipo array"
    | op == "." = do
        tStruct <- checkExpr e1
        case e2 of
            EVar fieldName -> do
                case tStruct of
                    TArray _ -> 
                        if fieldName == "size" 
                        then return TInt 
                        else throwError "Arrays possuem apenas a propriedade .size"
                    TStruct sName -> do
                        st <- get
                        case Map.lookup sName (structs st) of
                            Just fields -> case Map.lookup fieldName fields of
                                Just tField -> return tField
                                Nothing -> throwError $ "Campo " ++ fieldName ++ " não encontrado em " ++ sName
                            Nothing -> throwError $ "Struct " ++ sName ++ " não definida"
                    _ -> throwError "Acesso a campo requer struct ou array"
            _ -> throwError "Acesso a campo espera identificador"

    | op `elem` ["+", "-", "*", "/"] = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        if t1 == t2 && (t1 == TInt || t1 == TFloat) then return t1
        else throwError $ "Incompatibilidade aritmética: " ++ show t1 ++ " " ++ op ++ " " ++ show t2
    | op `elem` ["<", ">", "<=", ">="] = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        if t1 == t2 && (t1 == TInt || t1 == TFloat) then return TBool
        else throwError "Incompatibilidade de comparação"
    | op `elem` ["==", "!="] = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        if t1 == t2 then return TBool else throwError "Incompatibilidade de igualdade"
    | op `elem` ["&&", "||"] = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        expectType TBool t1
        expectType TBool t2
        return TBool
    | op == "=" = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        expectType t1 t2
        return t1
    | otherwise = throwError $ "Operador binário desconhecido: " ++ op

checkExpr (EPost e _) = checkExpr e
checkExpr (EPre _ e) = checkExpr e