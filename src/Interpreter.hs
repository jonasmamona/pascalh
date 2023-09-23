module Interpreter where

data TokenType = INTEGER | PLUS | MINUS | DIVISION | MULTIPLICATION | EOF | WHITESPACE | NOOP deriving (Show, Eq)

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
  | c == '*' = MULTIPLICATION
  | c == '/' = DIVISION
  | c == ' ' = WHITESPACE
  | c `elem` ['0' .. '9'] = INTEGER
  | otherwise = EOF

isCorrectOperation :: Token -> Token -> Bool
isCorrectOperation a b = case getTokenType a of
  PLUS -> case getTokenType b of
    INTEGER -> True
    WHITESPACE -> True
    _ -> False
  MINUS -> case getTokenType b of
    INTEGER -> True
    WHITESPACE -> True
    _ -> False
  MULTIPLICATION -> case getTokenType b of
    INTEGER -> True
    WHITESPACE -> True
    _ -> False
  DIVISION -> case getTokenType b of
    INTEGER -> True
    WHITESPACE -> True
    _ -> False
  INTEGER -> case getTokenType b of
    INTEGER -> False
    _ -> True
  EOF -> False
  WHITESPACE -> True
  NOOP -> True

tokenize :: String -> [Token]
tokenize [] = [Token EOF NoTokenValue]
tokenize list@(x : xs) = case getTokenTypeFromChar x of
  INTEGER -> Token INTEGER (TokenValueInteger $ read (takeWhile (`elem` ['0' .. '9']) list)) : tokenize (dropWhile (`elem` ['0' .. '9']) list)
  PLUS -> Token PLUS NoTokenValue : tokenize xs
  MINUS -> Token MINUS NoTokenValue : tokenize xs
  MULTIPLICATION -> Token MULTIPLICATION NoTokenValue : tokenize xs
  DIVISION -> Token DIVISION NoTokenValue : tokenize xs
  WHITESPACE -> tokenize xs
  NOOP -> tokenize xs
  EOF -> [Token EOF NoTokenValue]

removeNOOPTokens :: [Token] -> [Token]
removeNOOPTokens = foldr (\x acc -> if getTokenType x /= NOOP then x : acc else acc) []

validateTokens :: [Token] -> [Token]
validateTokens [] = [Token EOF NoTokenValue]
validateTokens (x : xs) = validate x (Token NOOP NoTokenValue) xs []
  where
    validate :: Token -> Token -> [Token] -> [Token] -> [Token]
    validate a b [] acc = if isCorrectOperation a b then acc ++ [a] ++ [b] else error "Syntax error - Dangling operator"
    validate a b (y : ys) acc =
      if isCorrectOperation a b
        then validate b y ys (acc ++ [a])
        else error "Syntax error - Dangling operator"

validateSyntax :: [Token] -> [Token]
validateSyntax = removeNOOPTokens . validateTokens

parse :: [Token] -> Integer
parse [] = 0
parse (x : xs) = case getTokenType x of
  INTEGER -> case getTokenType (head xs) of
    PLUS -> extractTokenValueInteger x + parse xs
    MINUS -> extractTokenValueInteger x - parse xs
    MULTIPLICATION -> extractTokenValueInteger x * parse xs
    DIVISION -> extractTokenValueInteger x `div` parse xs
    _ -> extractTokenValueInteger x
  _ -> parse xs

interpret :: String -> Integer
interpret = parse . validateSyntax . tokenize

sampleString :: String
sampleString = "1 + 2 + 3"