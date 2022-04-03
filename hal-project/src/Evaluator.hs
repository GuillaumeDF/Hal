module Evaluator where

import Parser
import Printer
import System.IO
import Debug.Trace

debug = flip trace

builtinSum :: LispValue -> LispValue
builtinSum (Builtins (Number expr1) (Number expr2)) = Number (show ((read expr1 :: Int) + (read expr2 :: Int)))
builtinSum (Builtins (expr1) (Builtins expr2 End)) = builtinSum $ Builtins (doEvaluation expr1) (doEvaluation expr2)
builtinSum _ = Error "Sum needs two arguments."

builtinDivision :: LispValue -> LispValue
builtinDivision (Builtins (Number expr1) (Number expr2)) = Number (show ((read expr1 :: Int) `div` (read expr2 :: Int)))
builtinDivision (Builtins (expr1) (Builtins expr2 End)) = builtinDivision $ Builtins (doEvaluation expr1) (doEvaluation expr2)
builtinDivision _ = Error "Operator division needs two arguments."

builtinSubstraction :: LispValue -> LispValue
builtinSubstraction (Builtins (Number expr1) (Number expr2)) = Number (show ((read expr1 :: Int) - (read expr2 :: Int)))
builtinSubstraction (Builtins (expr1) (Builtins expr2 End)) = builtinSubstraction $ Builtins (doEvaluation expr1) (doEvaluation expr2)
builtinSubstraction _ = Error "Operator substraction needs two arguments."

builtinMultiplication :: LispValue -> LispValue
builtinMultiplication (Builtins (Number expr1) (Number expr2)) = Number (show ((read expr1 :: Int) * (read expr2 :: Int)))
builtinMultiplication (Builtins (expr1) (Builtins expr2 End)) = builtinMultiplication $ Builtins (doEvaluation expr1) (doEvaluation expr2)
builtinMultiplication _ = Error "Operator multiplication needs two arguments."

builtinModulo :: LispValue -> LispValue
builtinModulo (Builtins (Number expr1) (Number expr2)) = Number (show ((read expr1 :: Int) `mod` (read expr2 :: Int)))
builtinModulo (Builtins (expr1) (Builtins expr2 End)) = builtinModulo $ Builtins (doEvaluation expr1) (doEvaluation expr2)
builtinModulo _ = Error "Operator modulo needs two arguments."

builtinLessThan :: LispValue -> LispValue
builtinLessThan (Builtins (Number expr1) (Number expr2)) = Bool ((read expr1 :: Int) < (read expr2 :: Int))
builtinLessThan (Builtins (expr1) (Builtins expr2 End)) = builtinLessThan $ Builtins (doEvaluation expr1) (doEvaluation expr2)
builtinLessThan _ = Error "Operator Less-Than needs two arguments."

builtinLambda :: LispValue -> LispValue
builtinLambda (Builtins expr1 (Builtins _ _)) = Atom "#<procedure>"
builtinLambda _ = Error "Lambda needs a list of parameters as first argument and an expresion."

builtinLet :: LispValue -> LispValue
builtinLet (Builtins expr1 expr2) = Builtins expr1 (doEvaluation expr2)
builtinLet _ = Error "Let needs two arguments."

builtinCond :: LispValue -> LispValue
builtinCond (Builtins expr1 expr2) = Builtins expr1 (doEvaluation expr2)
builtinCond _ = Error "Cond needs two arguments."

builtinAtom :: LispValue -> LispValue
builtinAtom (Builtins (Atom _) _) = Bool True
builtinAtom (Builtins (End) _) = Bool True
builtinAtom _ = Bool False

builtinList :: LispValue -> LispValue
builtinList (Builtins expr End) = expr
builtinList (Builtins (expr1) (Builtins expr2 rest)) = Builtins expr1 (doEvaluation (Builtins expr2 rest))
builtinList _ = Error "List needs at least one argument."

builtinEqual :: LispValue -> LispValue
builtinEqual (Builtins (Atom expr1) (Builtins (Atom expr2) End)) = Bool (expr1 == expr2)
builtinEqual (Builtins (Number expr1) (Builtins (Number expr2) End)) = Bool (expr1 == expr2)
builtinEqual (Builtins (Bool expr1) (Builtins (Bool expr2) End)) = Bool (expr1 == expr2)
builtinEqual (Builtins End (Builtins End End)) = Bool True
builtinEqual (Builtins expr1 (Builtins expr2 End)) = Bool ((getResultEvaluation (doEvaluation expr1)) == (getResultEvaluation( doEvaluation expr2)))
builtinEqual _ = Error "Equal needs two arguments."

builtinDefine :: LispValue -> LispValue
builtinDefine (Builtins expr (Builtins _ End)) = expr
builtinDefine (Builtins (Builtins expr _) _) = expr
builtinDefine _ = Error "Define needs two arguments."

builtinCdr :: LispValue -> LispValue
builtinCdr (Builtins (Builtins (Atom "cons") (Builtins _ (Builtins expr _))) _) = expr
builtinCdr (Builtins (Builtins _ (Builtins expr rest)) _) = Builtins expr (doEvaluation rest)
builtinCdr _ = Error "Cdr takes a cons as argument."

builtinCar :: LispValue -> LispValue
builtinCar (Builtins (Builtins (Atom "cons") (Builtins expr _)) _) = expr
builtinCar (Builtins (Builtins expr rest) _) = expr
builtinCar _ = Error "Car takes a cons as argument."

builtinCons :: LispValue -> LispValue
builtinCons (Builtins (expr1) (Builtins expr2 End)) = Builtins expr1 (doEvaluation expr2)
builtinCons _ = Error "Cons needs two arguments."

builtinQuote :: LispValue -> LispValue
builtinQuote (Builtins expr End) = expr
builtinQuote _ = Error "Quote needs one argument."

isBuiltins :: LispValue -> String -> Bool
isBuiltins (Builtins (Atom expr) _) atom = expr == atom
isBuiltins _ _ = False

doEvaluation :: LispValue -> LispValue
doEvaluation (End) = End
doEvaluation (Bool bool) = Bool bool
doEvaluation (Atom expr) = Atom expr
doEvaluation (Number expr) = Number expr
doEvaluation (Error error) = Error error
doEvaluation builtin@(Builtins content rest)
  | isBuiltins builtin "quote" = builtinQuote rest
  | isBuiltins builtin "cons" = builtinCons rest
  | isBuiltins builtin "car" = builtinCar rest
  | isBuiltins builtin "cdr" = builtinCdr rest
  | isBuiltins builtin "define" = builtinDefine rest
  | isBuiltins builtin "eq?" = builtinEqual rest
  | isBuiltins builtin "list" = builtinList rest
  | isBuiltins builtin "atom?" = builtinAtom rest
  | isBuiltins builtin "cond" = builtinCond rest
  | isBuiltins builtin "let" = builtinLet rest
  | isBuiltins builtin "lambda" = builtinLambda rest
  | isBuiltins builtin "+" = builtinSum rest
  | isBuiltins builtin "div" = builtinDivision rest
  | isBuiltins builtin "-" = builtinSubstraction rest
  | isBuiltins builtin "*" = builtinMultiplication rest
  | isBuiltins builtin "mod" = builtinModulo rest
  | isBuiltins builtin "<" = builtinLessThan rest
  | otherwise = Builtins content (doEvaluation rest)

getResultEvaluation :: LispValue -> [String]
getResultEvaluation (Atom expr) = [expr]
getResultEvaluation (Number expr) = [expr]
getResultEvaluation (Bool True) = ["#t"]
getResultEvaluation (Bool False) = ["#f"]
getResultEvaluation (End) = []
getResultEvaluation (Error error) = ["*** ERROR : " ++ error]
getResultEvaluation (Builtins expr rest) = (concat (getResultEvaluation expr)):(getResultEvaluation rest)

evaluator :: [LispValue] -> IO ()
evaluator [] = putStr ""
evaluator (expr:rest) = let result = getResultEvaluation $ doEvaluation expr in
                      case length result of
                      1 -> printResult result >> putStrLn "" >> evaluator rest
                      otherwise -> putStr "(" >> printResult result >> putStrLn ")" >> evaluator rest
