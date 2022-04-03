{-# LANGUAGE ScopedTypeVariables #-}

module ManagerFile where

import Parser
import Control.Exception (try, SomeException)

myReadFile :: String -> IO (Maybe ([String]))
myReadFile "" = pure Nothing
myReadFile file = try (readFile file) >>= \result -> case result of
                                                      Left (e :: SomeException) -> pure Nothing
                                                      Right content -> pure $ Just (lines content)

getContent :: [Expression] -> Maybe ([[LispValue]])
getContent [] = Just ([])
getContent (expr:rest) = case readExpr expr of
                          Just (lispValue) -> (<$>) (\(expr1) -> (lispValue:expr1)) (getContent rest)
                          otherwise -> Just ([])

manager :: [String] -> Bool -> Int -> IO ([[[LispValue]]], Bool)
manager _ _ 0 = pure $ ([], True)
manager [] iMode length = pure $ ([], iMode)
manager (args:rest) iMode length
  | args == "-i" = manager rest True length
  | otherwise =  myReadFile args >>= \contentFile -> case contentFile of
                                                      Just (content) -> let Just (result) = (getContent content) in
                                                                        (<$>) (\(expr, iMode) -> (result:expr, iMode)) (manager rest iMode length)
                                                      otherwise -> (<$>) (\(expr, iMode)
                                                                -> ([[Error "The file does not exist."]]:expr, iMode)) (manager rest iMode length)
