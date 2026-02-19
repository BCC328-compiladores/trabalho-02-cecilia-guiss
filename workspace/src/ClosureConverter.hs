module ClosureConverter where

import Parser
import Types
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Data.List (nub)

-- | Estado do conversor para gerar nomes únicos e acumular definições elevadas
data CCState = CCState
    { nextId      :: Int
    , liftedDefs  :: [Definition]
    , globalNames :: Set String
    }

type CCM a = State CCState a

-- | Ponto de entrada principal
convertProgram :: [Definition] -> [Definition]
convertProgram defs = 
    let initialGlobals = Set.fromList [getDefName d | d <- defs]
        (newDefs, finalState) = runState (mapM convertDef defs) (CCState 0 [] initialGlobals)
    in liftedDefs finalState ++ newDefs

getDefName (DefFunc f) = fName f
getDefName (DefStruct s) = sName s
getDefName (DefGlobalLet l) = lName l

-- | Gera um nome único para funções elevadas ou structs de ambiente
freshName :: String -> CCM String
freshName prefix = do
    st <- get
    let name = prefix ++ "_" ++ show (nextId st)
    put st { nextId = nextId st + 1 }
    return name

-- | Converte uma definição (topo de nível)
convertDef :: Definition -> CCM Definition
convertDef (DefFunc f) = DefFunc <$> convertFunc Set.empty f
convertDef (DefStruct s) = return (DefStruct s)
convertDef (DefGlobalLet l) = do
    newVal <- case lval l of
                Just e -> Just <$> convertExpr Set.empty e
                Nothing -> return Nothing
    return $ DefGlobalLet l { lval = newVal }

-- | Converte uma função
convertFunc :: Set String -> Func -> CCM Func
convertFunc bound f = do
    let localBound = Set.union bound (Set.fromList $ map fst (fParams f))
    newBody <- mapM (convertStmt localBound) (fBody f)
    return f { fBody = newBody }

-- | Converte um comando
convertStmt :: Set String -> Stmt -> CCM Stmt
convertStmt bound stmt = case stmt of
    SLet name t mE -> do
        newE <- case mE of
                  Just e -> Just <$> convertExpr bound e
                  Nothing -> return Nothing
        -- Adicionamos o nome da variável ao bound para os próximos stmts? 
        -- Em um bloco, variáveis posteriores podem usar as anteriores.
        -- No entanto, convertStmt opera em um único stmt. O mapM em convertFunc não propaga o bound.
        -- Mas convertExpr já recebe o bound atual.
        return $ SLet name t newE
    SReturn e -> SReturn <$> convertExpr bound e
    SIf c t e -> SIf <$> convertExpr bound c <*> mapM (convertStmt bound) t <*> mapM (mapM (convertStmt bound)) e
    SWhile c b -> SWhile <$> convertExpr bound c <*> mapM (convertStmt bound) b
    SFor i c inc b -> SFor <$> mapM (convertStmt bound) i <*> convertExpr bound c <*> mapM (convertExpr bound) inc <*> mapM (convertStmt bound) b
    SExpr e -> SExpr <$> convertExpr bound e
    SDef (DefFunc f) -> do
        lambda <- convertExpr bound (ELambda f)
        return $ SLet (fName f) (Just "func_ptr") (Just lambda)
    SDef d -> return (SDef d)

-- | Converte uma expressão
convertExpr :: Set String -> Expr -> CCM Expr
convertExpr bound expr = case expr of
    ELambda f -> liftLambda bound f
    ECall n args -> ECall n <$> mapM (convertExpr bound) args
    EBin op e1 e2 -> EBin op <$> convertExpr bound e1 <*> convertExpr bound e2
    EArray es -> EArray <$> mapM (convertExpr bound) es
    ENew t e -> ENew t <$> convertExpr bound e
    EPost e op -> EPost <$> convertExpr bound e <*> return op
    EPre op e -> EPre op <$> convertExpr bound e
    EVar v -> return $ EVar v
    _ -> return expr

-- | Transforma uma lambda/função aninhada em uma estrutura de Closure
liftLambda :: Set String -> Func -> CCM Expr
liftLambda bound f = do
    st <- get
    -- Variáveis livres são aquelas que estão no 'bound' (escopos pais) mas NÃO no parâmetro ou globais
    let params = Set.fromList (map fst (fParams f))
    let allUsed = freeVarsFunc Set.empty f
    let frees = Set.toList $ Set.filter (\v -> Set.member v bound && not (Set.member v (globalNames st))) allUsed
    
    fNameNew <- freshName ("lifted_" ++ fName f)
    envStructName <- freshName "Env"
    
    let envFields = [(v, "var") | v <- frees] 
    let structDef = DefStruct (Struct envStructName envFields)
    
    let envParam = ("_env", envStructName)
    -- Importante: no corpo elevado, as variáveis livres agora vêm de _env.v
    -- Mas as variáveis locais e parâmetros de f continuam normais.
    let bodyWithEnv = translateFreeVars frees "_env" (fBody f)
    
    liftedF <- convertFunc Set.empty (f { fName = fNameNew, fParams = envParam : fParams f, fBody = bodyWithEnv })
    modify $ \s -> s { liftedDefs = liftedDefs s ++ [structDef, DefFunc liftedF] }
    
    return $ ECall "make_closure" [EVar fNameNew, createEnvObject envStructName frees]

-- | Auxiliar: Variáveis usadas em uma função que não são definidas nela
freeVarsFunc :: Set String -> Func -> Set String
freeVarsFunc localBound f = 
    let newBound = Set.union localBound (Set.fromList (map fst (fParams f)))
    in Set.unions (map (freeVarsStmt newBound) (fBody f))

freeVarsStmt :: Set String -> Stmt -> Set String
freeVarsStmt bound stmt = case stmt of
    SLet n _ mE -> 
        let used = maybe Set.empty (freeVarsExpr bound) mE
        in used -- Adicionamos 'n' ao bound para o RESTO do bloco? 
                -- Sim, mas aqui estamos analisando um stmt isolado.
    SReturn e -> freeVarsExpr bound e
    SIf c t e -> Set.unions [freeVarsExpr bound c, Set.unions (map (freeVarsStmt bound) t), maybe Set.empty (Set.unions . map (freeVarsStmt bound)) e]
    SWhile c b -> freeVarsExpr bound c `Set.union` Set.unions (map (freeVarsStmt bound) b)
    SFor i c inc b -> Set.unions [maybe Set.empty (freeVarsStmt bound) i, freeVarsExpr bound c, maybe Set.empty (freeVarsExpr bound) inc, Set.unions (map (freeVarsStmt bound) b)]
    SExpr e -> freeVarsExpr bound e
    SDef (DefFunc f) -> freeVarsFunc bound f
    _ -> Set.empty

freeVarsExpr :: Set String -> Expr -> Set String
freeVarsExpr bound expr = case expr of
    EVar v -> if Set.member v bound then Set.empty else Set.singleton v
    ECall _ args -> Set.unions (map (freeVarsExpr bound) args)
    EBin _ e1 e2 -> freeVarsExpr bound e1 `Set.union` freeVarsExpr bound e2
    EArray es -> Set.unions (map (freeVarsExpr bound) es)
    ENew _ e -> freeVarsExpr bound e
    EPost e _ -> freeVarsExpr bound e
    EPre _ e -> freeVarsExpr bound e
    ELambda f -> freeVarsFunc bound f
    _ -> Set.empty

-- | Substituição recursiva em todos os tipos de Stmt e Expr
substStmt :: [String] -> String -> Stmt -> Stmt
substStmt frees env stmt = case stmt of
    SReturn e -> SReturn (substExpr frees env e)
    SLet n t mE -> SLet n t (fmap (substExpr frees env) mE)
    SIf c t e -> SIf (substExpr frees env c) (map (substStmt frees env) t) (fmap (map (substStmt frees env)) e)
    SWhile c b -> SWhile (substExpr frees env c) (map (substStmt frees env) b)
    SFor i c inc b -> SFor (fmap (substStmt frees env) i) (substExpr frees env c) (fmap (substExpr frees env) inc) (map (substStmt frees env) b)
    SExpr e -> SExpr (substExpr frees env e)
    SDef d -> SDef d -- Definições internas já foram tratadas via ELambda
    _ -> stmt

substExpr :: [String] -> String -> Expr -> Expr
substExpr frees env expr = case expr of
    EVar v -> if v `elem` frees 
              then EBin "." (EVar env) (EVar v)
              else EVar v
    ECall n args -> ECall n (map (substExpr frees env) args)
    EBin op e1 e2 -> EBin op (substExpr frees env e1) (substExpr frees env e2)
    EArray es -> EArray (map (substExpr frees env) es)
    ENew t e -> ENew t (substExpr frees env e)
    EPost e op -> EPost (substExpr frees env e) op
    EPre op e -> EPre op (substExpr frees env e)
    ELambda f -> ELambda f -- Lambdas internos serão convertidos na sua própria fase de lifting
    _ -> expr

translateFreeVars :: [String] -> String -> [Stmt] -> [Stmt]
translateFreeVars frees envName body = map (substStmt frees envName) body

createEnvObject :: String -> [String] -> Expr
createEnvObject structName frees = 
    ECall ("new_" ++ structName) (map EVar frees)
