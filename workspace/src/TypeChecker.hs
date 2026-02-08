module TypeChecker where
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Types
import Parser

data Env = Env { vars :: Map String Type, context :: [Map String Type] }
type CheckM a = ExceptT String (State Env) a

enterScope :: CheckM ()
enterScope = modify (\s -> s { context = vars s : context s })

exitScope :: CheckM ()
exitScope = do
    st <- get
    case context st of
        (old:rest) -> put st { vars = old, context = rest }
        [] -> throwError "Escopo inválido"

checkExpr :: Expr -> CheckM Type
checkExpr (EInt _) = return TInt
checkExpr (EVar n) = do
    st <- get
    case Map.lookup n (vars st) of
        Just t -> return t
        Nothing -> throwError "Variável não declarada"
checkExpr _ = throwError "Não implementado"
