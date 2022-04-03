module Printer where

import Parser

printResult :: [String] -> IO ()
printResult [] = putStr ""
printResult (value:[]) = putStr value
printResult (value:rest) = putStr (value ++ " ") >> printResult rest

debug :: LispValue -> IO ()
debug (End) = putStrLn "End"
debug (Bool True) = putStrLn "#t"
debug (Bool False) = putStrLn "#f"
debug (Atom expr) = putStrLn expr
debug (Number expr) = putStrLn expr
debug (Error error) = putStrLn ("*** ERROR : " ++ error)
debug (Builtins content rest) = putStrLn "Builtins" >> debug rest

printDebug :: [LispValue] -> IO ()
printDebug [] = putStr ""
printDebug (expr:rest) = debug expr >> printDebug rest
