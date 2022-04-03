module Error where

import Parser

catchErrorBuiltins :: LispValue -> Maybe (LispValue)
catchErrorBuiltins expr = case expr of
                          Error error -> Just (expr)
                          Builtins content rest -> case content of
                                                    Error error -> Just (expr)
                                                    otherwise -> catchErrorBuiltins rest
                          otherwise -> Nothing

catchError :: [LispValue] -> Maybe (LispValue)
catchError [] = Nothing
catchError (expr:rest) = case expr of
                          Error error -> Just (expr)
                          Builtins content rest -> catchErrorBuiltins expr
                          otherwise -> catchError rest
