module Parser where

type Expression = String
type Parser result = Expression -> Maybe (result, String)

data LispValue = Atom String
              | Builtins LispValue LispValue
              | End
              | Number String
              | Bool Bool
              | Error String

(<|>) :: Parser a -> Parser a -> Parser a
parser1 <|> parser2 = \expr -> let result = parser1 expr in
                                case result of
                                Just _  -> result
                                Nothing -> parser2 expr

isExist :: Expression -> Char -> Maybe (Bool)
isExist "" _ = Nothing
isExist (firstChar:rest) charToFind
  | firstChar == charToFind = Just (True)
  | otherwise = isExist rest charToFind

isSymbol :: Parser Char
isSymbol "" = Nothing
isSymbol (char:rest) = case isExist "!#$%&|*+-/:<=>?@^_~" char of
                        Just True -> Just (char, rest)
                        otherwise -> Nothing

isDigit :: Parser Char
isDigit "" = Nothing
isDigit (char:rest) = case isExist "0123456789" char of
                      Just True -> Just (char, rest)
                      otherwise -> Nothing

isLetter :: Parser Char
isLetter "" = Nothing
isLetter (char:rest) = case isExist "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ" char of
                        Just True -> Just (char, rest)
                        otherwise -> Nothing

isString :: Parser Char
isString = isLetter <|> isDigit <|> isSymbol

isAtom :: Parser Char
isAtom = isLetter <|> isSymbol

tokenizeAtom :: (Expression, Expression) -> Maybe (LispValue, Expression)
tokenizeAtom (expr, rest)
  | "#t" == expr = Just (Bool True, rest)
  | "#f" == expr = Just (Bool False, rest)
  | otherwise = Just (Atom expr, rest)

parseString :: Parser Expression
parseString expr = case isString expr of
                    Just (char, rest) -> (<$>) (\(res,rest2) -> (char:res, rest2)) (parseString rest)
                    Nothing -> Just ("", expr)

parseNumber :: Parser Expression
parseNumber expr = case isDigit expr of
                    Just (char, rest) -> (<$>) (\(res,rest2) -> (char:res, rest2)) (parseNumber rest)
                    Nothing -> Just ("", expr)

parseList :: Expression -> Maybe (LispValue, Expression)
parseList "" = Just (Error "It seems that a parenthesis is missing.", "")
parseList expr@(char:rest)
  | char == ')' = Just (End, rest)
  | otherwise = let result = parser expr
                in case result of
                    Just (expr1, rest1) -> let result = parseList rest1
                                            in case result of
                                                Just (expr2, rest2) -> Just (Builtins expr1 expr2, rest2)
                                                otherwise -> Nothing
                    otherwise -> Nothing

parser :: Expression -> Maybe (LispValue, Expression)
parser "" = Just (End, "")
parser expr@(char:rest)
  | char == '(' = parseList rest
  | isDigit expr /= Nothing = parseNumber expr >>= \(expr, rest) -> Just (Number expr, rest)
  | isAtom expr /= Nothing = parseString expr >>= tokenizeAtom
  | otherwise = parser rest

readExpr :: Expression -> Maybe ([LispValue])
readExpr "" = Just ([])
readExpr input = case parser input of
                  Just (expr, rest) -> (<$>) (\(expr1) -> (expr:expr1)) (readExpr rest)
                  otherwise -> Nothing
