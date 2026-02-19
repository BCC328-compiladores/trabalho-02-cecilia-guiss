module Infer (
    Scheme(..), Subst,
    ftvType, ftvScheme, applySubstType, applySubstScheme,
    freshTVarName, instantiate, generalize,
    unify, composeSubst,
    runInfer
    ) where

import Types
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State
import Control.Monad.Except

-- Polymorphic scheme
data Scheme = Forall [String] Type
  deriving (Eq, Show)

type Subst = Map String Type

emptySubst :: Subst
emptySubst = Map.empty

-- Free type variables
ftvType :: Type -> Set String
ftvType TInt = Set.empty
ftvType TFloat = Set.empty
ftvType TString = Set.empty
ftvType TBool = Set.empty
ftvType TVoid = Set.empty
ftvType TNull = Set.empty
ftvType (TVar n) = Set.singleton n
ftvType (TArray t) = ftvType t
ftvType (TStruct _) = Set.empty
ftvType (TFunc args ret) = Set.unions (map ftvType args) `Set.union` ftvType ret

ftvScheme :: Scheme -> Set String
ftvScheme (Forall vars t) = ftvType t `Set.difference` Set.fromList vars

-- Apply substitution to types
applySubstType :: Subst -> Type -> Type
applySubstType s (TVar n) = Map.findWithDefault (TVar n) n s
applySubstType s (TArray t) = TArray (applySubstType s t)
applySubstType s (TFunc args ret) = TFunc (map (applySubstType s) args) (applySubstType s ret)
applySubstType _ t = t

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Forall vars t) = Forall vars (applySubstType s' t)
  where s' = foldr Map.delete s vars

-- Fresh type variable name generator
-- We expose a helper that runs in State Int
freshTVarName :: State Int String
freshTVarName = do
    n <- get
    put (n + 1)
    return $ "t" ++ show n

-- Instantiate a scheme into a concrete type by replacing quantified vars with fresh TVars
instantiate :: Scheme -> State Int Type
instantiate (Forall vars t) = do
    freshNames <- mapM (const freshTVarName) vars
    let s = Map.fromList (zip vars (map TVar freshNames))
    return $ applySubstType s t

-- Generalize a type with respect to a set of type variables in the environment
generalize :: Set String -> Type -> Scheme
generalize envFtv t = Forall varsToQuant (t)
  where
    tFtv = ftvType t
    varsToQuant = Set.toList (tFtv `Set.difference` envFtv)

-- Subst composition: s1 `compose` s2 means apply s1 then s2
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (applySubstType s1) s2 `Map.union` s1

-- Unification with occurs-check
type Infer a = ExceptT String (State Int) a

occursCheck :: String -> Type -> Bool
occursCheck a t = a `Set.member` ftvType t

varBind :: String -> Type -> Infer Subst
varBind a t
    | t == TVar a = return Map.empty
    | occursCheck a t = throwError $ "Occurs check: " ++ a ++ " in " ++ show t
    | otherwise = return $ Map.singleton a t

unify :: Type -> Type -> Infer Subst
unify (TFunc as1 r1) (TFunc as2 r2)
    | length as1 == length as2 = do
        s1s <- unifyList as1 as2
        let s = foldl composeSubst Map.empty s1s
        s2 <- unify (applySubstType s r1) (applySubstType s r2)
        return (composeSubst s2 s)
    | otherwise = throwError "Cannot unify functions with different arity"
unify (TArray t1) (TArray t2) = unify t1 t2
unify (TVar a) t = varBind a t
unify t (TVar a) = varBind a t
unify TInt TInt = return Map.empty
unify TFloat TFloat = return Map.empty
unify TString TString = return Map.empty
unify TBool TBool = return Map.empty
unify TVoid TVoid = return Map.empty
unify TNull TNull = return Map.empty
unify (TStruct n1) (TStruct n2)
    | n1 == n2 = return Map.empty
    | otherwise = throwError $ "Cannot unify structs " ++ n1 ++ " and " ++ n2
unify a b = throwError $ "Cannot unify " ++ show a ++ " with " ++ show b

unifyList :: [Type] -> [Type] -> Infer [Subst]
unifyList [] [] = return []
unifyList (x:xs) (y:ys) = do
    s1 <- unify x y
    rest <- unifyList (map (applySubstType s1) xs) (map (applySubstType s1) ys)
    return (s1 : rest)
unifyList _ _ = throwError "unifyList: different lengths"


runInfer :: Infer a -> Int -> (Either String a, Int)
runInfer m st = let (res, st') = runState (runExceptT m) st in (res, st')

