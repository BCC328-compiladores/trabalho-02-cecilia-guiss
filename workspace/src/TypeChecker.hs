module TypeChecker where
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Types
import Parser

data Env = Env { vars :: Map String Type }
type CheckM a = ExceptT String (State Env) a

checkProgram :: [Definition] -> Either String ()
checkProgram _ = Right () -- Esqueleto inicial

checkExpr :: Expr -> CheckM Type
checkExpr (EInt _) = return TInt
checkExpr (EFloat _) = return TFloat
checkExpr _ = throwError "Não implementado"
