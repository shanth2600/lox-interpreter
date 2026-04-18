{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, exitFailure, ExitCode(ExitSuccess, ExitFailure), exitSuccess)
import System.IO (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode(NoBuffering), readFile)

import Lexer
import Parser (expr, parseTokens)
import Text.Parsec (ParseError, SourcePos)
import AST (SomeExp)

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    args <- getArgs
    case args of
        (command : filename : _) -> do
            fileContents <- readFile filename
            let lexResults = tokenize filename fileContents
            case command of
                "tokenize" -> handleLexResult lexResults
                "parse"    -> handleParseResult $ 
                                parseTokens $ lexTokens $ lexResults
                _          -> hPutStrLn stderr ("Unknown command: " ++ command) >>
                              exitFailure
        _ -> do
            hPutStrLn stderr "Usage: ./your_program.sh tokenize <filename>"
            exitFailure

handleParseResult :: Either ParseError (SomeExp SourcePos) -> IO ()
handleParseResult parseRes = either (hPutStrLn stderr . show) (putStrLn . show) parseRes

lexTokens :: [LexResult] -> [Token SourcePos]
lexTokens res = [ t | (LexToken t) <- res]

handleLexResult :: [LexResult] -> IO ()
handleLexResult res
  | null errors = displayTokens >> exitSuccess
  | otherwise   = do
    displayTokens
    hPutStrLn stderr (diplayResult errors)
    exitWith (ExitFailure 65)
  where
    displayTokens = putStr (diplayResult tokens)
    errors = [ e | e@(LexError {}) <- res]
    tokens = [ t | t@(LexToken {}) <- res]
    diplayResult :: [LexResult] -> String
    diplayResult = unlines . map show