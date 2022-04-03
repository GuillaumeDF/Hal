module Main where

import Repl
import Parser
import Printer
import Error
import Evaluator
import ManagerFile
import System.Environment
import System.Exit

initHal :: [LispValue] -> Bool -> IO Bool
initHal listValue True = evaluator listValue >> repl listValue >> return True
initHal listValue _ = case (catchError listValue) of
                      Just (error) -> Printer.debug error >> return False
                      otherwise -> evaluator listValue >> return True

main :: IO ()
main = getArgs >>= \args -> (manager args False (length args)) >>= \(content, iMode)
                         -> (initHal (concat (concat content)) iMode) >>= \result
                         -> case result of
                         False -> exitWith $ ExitFailure 84
                         True -> exitSuccess
