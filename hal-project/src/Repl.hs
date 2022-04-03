module Repl where

import Error
import Printer
import Evaluator
import Parser
import System.IO

flushStdout :: String -> IO ()
flushStdout prompt = putStr prompt >> hFlush stdout

readPrompt :: String -> IO (String)
readPrompt prompt = flushStdout prompt >> getLine

repl :: [LispValue] -> IO ()
repl lispValue = (readPrompt "> ") >>= \line -> case line of
                                                "quit" -> putStr ""
                                                otherwise -> case (readExpr line) of
                                                              Just (content) -> case (catchError content) of
                                                                                Just (error) -> Printer.debug error >> repl []
                                                                                otherwise -> evaluator content >> repl []
                                                              otherwise -> repl []
