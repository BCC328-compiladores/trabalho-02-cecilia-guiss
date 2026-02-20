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
    let localParams = Set.fromList $ map fst (fParams f)
    -- O 'bound' para o corpo começa com o bound externo + parâmetros locais
    -- No entanto, para análise de captura de lambdas internos EXATAMENTE NESSE NÍVEL,
    -- precisamos saber o que foi definido aqui.
    newBody <- convertBlock (Set.union bound localParams) (fBody f)
    return f { fBody = newBody }

-- | Converte um bloco de comandos, propagando variáveis definidas em 'let'
convertBlock :: Set String -> [Stmt] -> CCM [Stmt]
convertBlock _ [] = return []
convertBlock bound (s:ss) = do
    newS <- convertStmt bound s
    let newBound = case s of
                     SLet name _ _ -> Set.insert name bound
                     _ -> bound
    newSS <- convertBlock newBound ss
    return (newS : newSS)

-- | Converte um comando
convertStmt :: Set String -> Stmt -> CCM Stmt
convertStmt bound stmt = case stmt of
    SLet name t mE -> do
        newE <- case mE of
                  Just e -> Just <$> convertExpr bound e
                  Nothing -> return Nothing
        return $ SLet name t newE
    SReturn e -> SReturn <$> convertExpr bound e
    SIf c t e -> SIf <$> convertExpr bound c <*> convertBlock bound t <*> mapM (convertBlock bound) e
    SWhile c b -> SWhile <$> convertExpr bound c <*> convertBlock bound b
    SFor i c inc b -> SFor <$> mapM (convertStmt bound) i <*> convertExpr bound c <*> mapM (convertExpr bound) inc <*> convertBlock bound b
    SExpr e -> SExpr <$> convertExpr bound e
    SDef (DefFunc f) -> do
        lambda <- convertExpr bound (ELambda f)
        return $ SLet (fName f) (Just "func_ptr") (Just lambda)
    SDef d -> return (SDef d)

-- | Converte uma expressão (Aqui acontece a mágica)
convertExpr :: Set String -> Expr -> CCM Expr
convertExpr bound expr = case expr of
    ELambda f -> liftLambda bound f
    ECall n args -> ECall n <$> mapM (convertExpr bound) args
    EBin op e1 e2 -> EBin op <$> convertExpr bound e1 <*> convertExpr bound e2
    EArray es -> EArray <$> mapM (convertExpr bound) es
    ENew t e -> ENew t <$> convertExpr bound e
    EPost e op -> EPost <$> convertExpr bound e <*> return op
    EPre op e -> EPre op <$> convertExpr bound e
    _ -> return expr

-- | Transforma uma lambda/função aninhada em uma estrutura de Closure
liftLambda :: Set String -> Func -> CCM Expr
liftLambda bound f = do
    st <- get
    let params = Set.fromList (map fst (fParams f))
    -- Variáveis usadas que NÃO são parâmetros locais
    let allUsed = freeVarsFunc Set.empty f 
    -- Variáveis que são livres em relação a 'f' MAS estão no 'bound' (escopos pais)
    -- E que NÃO são globais (funções de topo ou structs)
    let frees = Set.toList $ Set.filter (\v -> Set.member v bound && Set.notMember v (globalNames st)) allUsed
    
    -- 1. Cria nome para a função elevada e a struct do ambiente
    fNameNew <- freshName ("lifted_" ++ fName f)
    envStructName <- freshName "Env"
    
    -- 2. Cria a struct para o ambiente (capturas)
    let envFields = [(v, "var") | v <- frees] 
    let structDef = DefStruct (Struct envStructName envFields)
    
    -- 3. Modifica o corpo da função para acessar variáveis via '_env'
    let envParam = ("_env", envStructName)
    let newParams = envParam : fParams f
    let bodyWithEnv = translateFreeVars frees "_env" (fBody f)
    
    -- 4. Adiciona a função elevada ao estado (vazia de bound externo pois foi elevada)
    liftedF <- convertFunc Set.empty (f { fName = fNameNew, fParams = newParams, fBody = bodyWithEnv })
    modify $ \s -> s { liftedDefs = liftedDefs s ++ [structDef, DefFunc liftedF] }
    
    -- 5. No local original, cria o objeto closure: { f: ptr, env: { v1, v2... } }
    return $ ECall "make_closure" [EVar fNameNew, createEnvObject envStructName frees]

-- | Auxiliar: Extrai variáveis consumidas em uma função que não são definidas nela
freeVarsFunc :: Set String -> Func -> Set String
freeVarsFunc _ f = 
    let localParams = Set.fromList (map fst (fParams f))
    in freeVarsBlock localParams (fBody f)

freeVarsBlock :: Set String -> [Stmt] -> Set String
freeVarsBlock _ [] = Set.empty
freeVarsBlock bound (s:ss) = 
    let usedS = freeVarsStmt bound s
        newBound = case s of
                     SLet n _ _ -> Set.insert n bound
                     _ -> bound
    in usedS `Set.union` freeVarsBlock newBound ss

freeVarsStmt :: Set String -> Stmt -> Set String
freeVarsStmt bound stmt = case stmt of
    SLet n _ mE -> maybe Set.empty (freeVarsExpr bound) mE
    SReturn e -> freeVarsExpr bound e
    SIf c t e -> Set.unions [freeVarsExpr bound c, Set.unions (map (freeVarsStmt bound) t), maybe Set.empty (Set.unions . map (freeVarsStmt bound)) e]
    SWhile c b -> freeVarsExpr bound c `Set.union` Set.unions (map (freeVarsStmt bound) b)
    SFor mi c minc b -> 
        let usedMi = maybe Set.empty (freeVarsStmt bound) mi
            initVars = case mi of
                         Just (SLet n _ _) -> Set.singleton n
                         _ -> Set.empty
            loopBound = Set.union bound initVars
        in Set.unions [ usedMi
                      , freeVarsExpr loopBound c
                      , maybe Set.empty (freeVarsExpr loopBound) minc
                      , Set.unions (map (freeVarsStmt loopBound) b)
                      ]
    SExpr e -> freeVarsExpr bound e
    SDef (DefFunc f) -> freeVarsFunc bound f
    _ -> Set.empty

freeVarsExpr :: Set String -> Expr -> Set String
freeVarsExpr bound expr = case expr of
    EVar v -> if Set.member v bound then Set.empty else Set.singleton v
    ECall n args -> 
        let usedArgs = Set.unions (map (freeVarsExpr bound) args)
            -- Se 'n' não estiver no bound local e não for print, ele pode ser uma captura (HOF) ou global
            usedFunc = if n == "print" || Set.member n bound then Set.empty else Set.singleton n
        in usedFunc `Set.union` usedArgs
    EBin op e1 e2 -> 
        if op == "."
        then freeVarsExpr bound e1 -- Acesso a campo não conta o campo como variável livre
        else freeVarsExpr bound e1 `Set.union` freeVarsExpr bound e2
    EArray es -> Set.unions (map (freeVarsExpr bound) es)
    ENew _ e -> freeVarsExpr bound e
    EPost e _ -> freeVarsExpr bound e
    EPre _ e -> freeVarsExpr bound e
    ELambda f -> freeVarsFunc bound f
    _ -> Set.empty

-- | Substitui variáveis livres pelo acesso ao campo no struct de ambiente
-- Nova estratégia: adicionamos 'let v = _env.v' no início do corpo para cada v em frees
translateFreeVars :: [String] -> String -> [Stmt] -> [Stmt]
translateFreeVars frees envName body = 
    let projections = [SLet v Nothing (Just (EBin "." (EVar envName) (EVar v))) | v <- frees]
    in projections ++ body

substStmt _ _ b = b

createEnvObject :: String -> [String] -> Expr
createEnvObject structName frees = 
    ECall ("new_" ++ structName) (map EVar frees)
