module Types where

import Data.List (intercalate, isSuffixOf)

data Type
    = TInt
    | TFloat
    | TString
    | TBool
    | TVoid
    | TArray Type
    | TStruct String
    | TFunc [Type] Type -- (Argumentos) -> Retorno
    | TNull 
    deriving (Eq)

instance Show Type where
    show TInt = "int"
    show TFloat = "float"
    show TString = "string"
    show TBool = "bool"
    show TVoid = "void"
    show (TArray t) = show t ++ "[]"
    show (TStruct n) = n
    show (TFunc args ret) = "(" ++ intercalate ", " (map show args) ++ ") -> " ++ show ret
    show TNull = "null"

-- Converte uma string no tipo correspondente
parseTypeStr "" = TInt -- Padrão para int se o tipo for omitido
parseTypeStr "int" = TInt
parseTypeStr "float" = TFloat
parseTypeStr "string" = TString
parseTypeStr "bool" = TBool
parseTypeStr "void" = TVoid
parseTypeStr s
    | null s = TInt
    | "[]" `isSuffixOf` s = TArray (parseTypeStr (take (length s - 2) s))
    | last s == ']' = 
        case span (/= '[') s of
            (base, _) -> TArray (parseTypeStr base)
    | "->" `isInfixOf` s = parseFuncType s
    | otherwise = TStruct s

-- Auxiliar para detecção de "->"
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = []
    tails xs = xs : tails (tail xs)

-- Parser básico para tipos de função: (a,b) -> c
parseFuncType :: String -> Type
parseFuncType s = 
    let (argsPart, retPart) = breakSubstring "->" s
        argsContent = extractArgs (trim argsPart)
        retStr = trim (drop 2 retPart)
    in TFunc (map parseTypeStr argsContent) (parseTypeStr retStr)

breakSubstring :: String -> String -> (String, String)
breakSubstring needle [] = ([], [])
breakSubstring needle str@(x:xs)
    | needle `isPrefixOf` str = ([], str)
    | otherwise = let (before, after) = breakSubstring needle xs in (x:before, after)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (n:ns) (h:hs) = n == h && isPrefixOf ns hs

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-- Extrai argumentos de uma string como "(a,b)"
extractArgs :: String -> [String]
extractArgs s = 
    let inner = if head s == '(' && last s == ')' then take (length s - 2) (tail s) else s
    in if null inner then [] else splitComma inner

splitComma :: String -> [String]
splitComma "" = []
splitComma s = 
    let (w, rest) = break (== ',') s
    in trim w : case rest of
        [] -> []
        (_:r) -> splitComma r
