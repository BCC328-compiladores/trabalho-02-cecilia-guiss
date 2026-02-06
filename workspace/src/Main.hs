module Main where

import System.Environment (getArgs)
import System.IO (hSetEncoding, stdin, stdout, stderr, hPutStrLn, utf8, openFile, hGetContents, IOMode(ReadMode))
import System.Exit (exitFailure) 
import Lexer (lexTokens)
import SLToken (SLToken)
import Parser (parseSL)
import Data.Tree (drawTree)

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
            handle <- openFile filename ReadMode
            hSetEncoding handle utf8
            content <- hGetContents handle
            case lexTokens content filename of
                Left err -> do
                    hPutStrLn stderr ("Lexer error:\n" ++ errorBundlePretty err)
                    exitFailure
                Right tokens -> mapM_ showToken tokens

        ["--parser", filename] -> do
            handle <- openFile filename ReadMode
            hSetEncoding handle utf8
            content <- hGetContents handle
            case lexTokens content filename of
                Left err -> do
                    hPutStrLn stderr ("Lexer error:\n" ++ errorBundlePretty err)
                    exitFailure
                Right tokens -> case parseSL tokens of
                    Left perr -> do
                        hPutStrLn stderr ("Parser error:\n" ++ perr)
                        exitFailure
                    Right ast -> putStrLn (drawTree (defsToTree ast))

        ["--pretty", filename] -> do
            handle <- openFile filename ReadMode
            hSetEncoding handle utf8
            content <- hGetContents handle
            case lexTokens content filename of
                Left err -> do
                    hPutStrLn stderr ("Lexer error:\n" ++ errorBundlePretty err)
                    exitFailure
                Right tokens -> case parseSL tokens of
                    Left perr -> do
                        hPutStrLn stderr ("Parser error:\n" ++ perr)
                        exitFailure
                    Right ast -> putDoc (vsep (map prettyDefinition ast) <> hardline)

        _ -> putStrLn "Usage: sl [--lexer|--parser|--pretty] <file>"

showToken :: SLToken -> IO ()
showToken tok = putDoc (prettyToken tok <> hardline)
