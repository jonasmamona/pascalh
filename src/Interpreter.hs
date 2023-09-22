module Interpreter where

data TokenType = INTEGER | PLUS | MINUS | EOF | WHITESPACE deriving (Show, Eq)

data TokenValue = TokenValueInteger Integer | TokenValueChar Char | TokenValueString String | NoTokenValue deriving (Show, Eq)

data Token = Token {getTokenType :: TokenType, getTokenValue :: TokenValue} deriving (Show, Eq)

extractTokenValueInteger :: Token -> Integer
extractTokenValueInteger (Token INTEGER (TokenValueInteger x)) = x
extractTokenValueInteger _ = error "Not an integer"

extractTokenValueChar :: Token -> Char
extractTokenValueChar (Token INTEGER (TokenValueChar x)) = x
extractTokenValueChar _ = error "Not a char"

extractTokenValueString :: Token -> String
extractTokenValueString (Token INTEGER (TokenValueString x)) = x
extractTokenValueString _ = error "Not a string"

getTokenTypeFromChar :: Char -> TokenType
getTokenTypeFromChar c
  | c == '+' = PLUS
  | c == '-' = MINUS
  | c == ' ' = WHITESPACE
  | c `elem` ['0' .. '9'] = INTEGER
  | otherwise = EOF

tokenize :: String -> [Token]
tokenize [] = [Token EOF NoTokenValue]
tokenize list@(x : xs) = case getTokenTypeFromChar x of
  INTEGER -> Token INTEGER (TokenValueInteger $ read (takeWhile (`elem` ['0' .. '9']) list)) : tokenize (dropWhile (`elem` ['0' .. '9']) list)
  PLUS -> Token PLUS NoTokenValue : tokenize xs
  MINUS -> Token MINUS NoTokenValue : tokenize xs
  WHITESPACE -> tokenize xs
  EOF -> [Token EOF NoTokenValue]

parse :: [Token] -> Integer
parse [] = 0
parse (x : xs) = case getTokenType x of
  INTEGER -> case getTokenType (head xs) of
    PLUS -> extractTokenValueInteger x + parse xs
    MINUS -> extractTokenValueInteger x - parse xs
    _ -> extractTokenValueInteger x
  _ -> parse xs

interpret :: String -> Integer
interpret = parse . tokenize