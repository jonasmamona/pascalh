module Interpreter where

data TokenType = INTEGER | PLUS | MINUS | DIVISION | MULTIPLICATION | EOF | WHITESPACE deriving (Show, Eq)

data TokenValue = TokenValueInteger Integer | TokenValueChar Char | TokenValueString String | TokenValueOperation | NoTokenValue deriving (Show, Eq)

data Token = Token {getTokenType :: TokenType, getTokenValue :: TokenValue} deriving (Show, Eq)

data OperationPriority = One | Two | Three deriving (Show, Eq)

data BinaryOperation = BinaryOperation Token Token Token OperationPriority deriving (Eq, Show)

doOperandTypesMatch :: Token -> Token -> Bool
doOperandTypesMatch x y =
  case getTokenType x of
    INTEGER ->
      case getTokenType y of
        INTEGER -> True
        _ -> False
    _ -> False

doOperandsMatchOperator :: Token -> Token -> Token -> Bool
doOperandsMatchOperator x y operator =
  case getTokenValue operator of
    TokenValueOperation -> case getTokenType operator of
      PLUS -> case getTokenType x of
        INTEGER -> doOperandTypesMatch x y
        _ -> False
      MINUS -> case getTokenType x of
        INTEGER -> doOperandTypesMatch x y
        _ -> False
      MULTIPLICATION -> case getTokenType x of
        INTEGER -> doOperandTypesMatch x y
        _ -> False
      DIVISION -> case getTokenType x of
        INTEGER -> doOperandTypesMatch x y
        _ -> False
      _ -> False
    _ -> error "The provided value is not an operator"

createBinaryOperation :: Token -> Token -> Token -> BinaryOperation
createBinaryOperation x y f =
  if doOperandsMatchOperator x y f
    then case getTokenType f of
      PLUS -> BinaryOperation x y f One
      MINUS -> BinaryOperation x y f One
      MULTIPLICATION -> BinaryOperation x y f Three
      DIVISION -> BinaryOperation x y f Three
      _ -> error "Unknown operation"
    else error "Operands do not match operator"

executeBinaryOperation :: BinaryOperation -> Integer
executeBinaryOperation (BinaryOperation x y f _) =
  case getTokenType f of
    PLUS -> extractTokenValueInteger x + extractTokenValueInteger y
    MINUS -> extractTokenValueInteger x + extractTokenValueInteger y
    MULTIPLICATION -> extractTokenValueInteger x * extractTokenValueInteger y
    DIVISION -> extractTokenValueInteger x `div` extractTokenValueInteger y
    _ -> error "unknown operation"

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

isAllowedSequence :: Token -> Token -> Bool
isAllowedSequence a b = case getTokenType a of
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

tokenize :: String -> [Token]
tokenize [] = [Token EOF NoTokenValue]
tokenize list@(x : xs) = case getTokenTypeFromChar x of
  INTEGER -> Token INTEGER (TokenValueInteger $ read (takeWhile (`elem` ['0' .. '9']) list)) : tokenize (dropWhile (`elem` ['0' .. '9']) list)
  PLUS -> Token PLUS TokenValueOperation : tokenize xs
  MINUS -> Token MINUS TokenValueOperation : tokenize xs
  MULTIPLICATION -> Token MULTIPLICATION TokenValueOperation : tokenize xs
  DIVISION -> Token DIVISION TokenValueOperation : tokenize xs
  WHITESPACE -> tokenize xs
  EOF -> [Token EOF NoTokenValue]

validateSyntax :: [Token] -> [Token]
validateSyntax [] = [Token EOF NoTokenValue]
validateSyntax (x : xs) = validate x xs []
  where
    validate :: Token -> [Token] -> [Token] -> [Token]
    validate a [] acc = acc ++ [a]
    validate a (y : ys) acc =
      if isAllowedSequence a y
        then validate y ys (acc ++ [a])
        else error "Syntax error - Dangling operator"

parse :: [Token] -> [BinaryOperation]
parse [] = []
parse (x : xs) = case getTokenType x of
  INTEGER -> case getTokenType (head xs) of
    PLUS -> createBinaryOperation x secondOperand operator : parse listStartingFromNextValue
    MINUS -> createBinaryOperation x secondOperand operator : parse listStartingFromNextValue
    MULTIPLICATION -> createBinaryOperation x secondOperand operator : parse listStartingFromNextValue
    DIVISION -> createBinaryOperation x secondOperand operator : parse listStartingFromNextValue
    _ -> parse xs
  _ -> parse xs
  where
    operator = head xs
    secondOperand = head $ tail $ take 2 xs
    listStartingFromNextValue = drop 1 xs

sampleString :: String
sampleString = "7 - 3 + 2 - 1"

myList :: [Token]
myList = validateSyntax $ tokenize sampleString