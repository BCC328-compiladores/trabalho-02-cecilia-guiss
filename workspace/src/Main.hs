module Main where

import System.Environment (getArgs)
import System.IO (hSetEncoding, stdin, stdout, stderr, hPutStrLn, utf8, openFile, hGetContents, IOMode(ReadMode))
import System.Exit (exitFailure) 
import Lexer (lexTokens)
import SLToken (SLToken)
import Parser (parseSL)
import Data.Tree (drawTree)
import Interpreter (runInterpreter)
import TypeChecker (checkProgram)
import ClosureConverter (convertProgram)

import ASTtoTree

import Text.Megaparsec (errorBundlePretty)
import Prettyprinter (hardline, vsep)
import Pretty (prettyToken, prettyDefinition)
import Prettyprinter.Render.Terminal (putDoc)

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    args <- getArgs
    case args of
        ["--lexer", filename] -> do
            content <- readFileUTF8 filename
            case lexTokens content filename of
                Left err -> hPutStrLn stderr (errorBundlePretty err) >> exitFailure
                Right tokens -> mapM_ showToken tokens

        ["--parser", filename] -> do
            content <- readFileUTF8 filename
            case lexTokens content filename of
                Left err -> hPutStrLn stderr (errorBundlePretty err) >> exitFailure
                Right tokens -> case parseSL tokens of
                    Left perr -> hPutStrLn stderr perr >> exitFailure
                    Right ast -> putStrLn (drawTree (defsToTree ast))

        ["--pretty", filename] -> do
            content <- readFileUTF8 filename
            case lexTokens content filename of
                Left err -> hPutStrLn stderr (errorBundlePretty err) >> exitFailure
                Right tokens -> case parseSL tokens of
                    Left perr -> hPutStrLn stderr perr >> exitFailure
                    Right ast -> putDoc (vsep (map prettyDefinition ast) <> hardline)

        ["--typecheck", filename] -> do
            content <- readFileUTF8 filename
            case lexTokens content filename of
                Left err -> hPutStrLn stderr (errorBundlePretty err) >> exitFailure
                Right tokens -> case parseSL tokens of
                    Left perr -> hPutStrLn stderr perr >> exitFailure
                    Right ast -> case checkProgram ast of
                        Left terr -> hPutStrLn stderr ("Type Error: " ++ terr) >> exitFailure
                        Right () -> putStrLn "OK"

        ["--closure", filename] -> do
            content <- readFileUTF8 filename
            case lexTokens content filename of
                Left err -> hPutStrLn stderr (errorBundlePretty err) >> exitFailure
                Right tokens -> case parseSL tokens of
                    Left perr -> hPutStrLn stderr perr >> exitFailure
                    Right ast -> case checkProgram ast of
                        Left terr -> hPutStrLn stderr ("Type Error: " ++ terr) >> exitFailure
                        Right () -> do
                            let converted = convertProgram ast
                            putStrLn (drawTree (defsToTree converted))

        ["--run", filename] -> do
            content <- readFileUTF8 filename
            case lexTokens content filename of
                Left err -> hPutStrLn stderr (errorBundlePretty err) >> exitFailure
                Right tokens -> case parseSL tokens of
                    Left perr -> hPutStrLn stderr perr >> exitFailure
                    Right ast -> case checkProgram ast of
                        Left terr -> hPutStrLn stderr ("Type Error: " ++ terr) >> exitFailure
                        Right () -> do
                            let converted = convertProgram ast
                            res <- runInterpreter converted
                            case res of
                                Left err -> hPutStrLn stderr ("Runtime Error: " ++ err) >> exitFailure
                                Right () -> return ()

        _ -> putStrLn "Usage: sl [--lexer|--parser|--pretty|--typecheck|--closure|--run] <file>"

readFileUTF8 :: FilePath -> IO String
readFileUTF8 f = do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    hGetContents h

showToken :: SLToken -> IO ()
showToken tok = putDoc (prettyToken tok <> hardline)
