module Interpreter where

data TokenType = INTEGER | PLUS | MINUS | DIVISION | MULTIPLICATION | EOF | WHITESPACE deriving (Show, Eq)

data TokenValue = TokenValueInteger Integer | TokenValueChar Char | TokenValueString String | TokenValueOperation | NoTokenValue deriving (Show, Eq)

data Token = Token {getTokenType :: TokenType, getTokenValue :: TokenValue} deriving Show

instance Eq Token where
  (Token a b) == (Token c d) = a == c && b == d

data OperationPriority = One | Two | Three deriving Show

instance Eq OperationPriority where
  One == One = True
  One == _ = False
  Two == Two = True
  Two == _ = False
  Three == Three = True
  Three == _ = False

instance Ord OperationPriority where
  compare One One = EQ
  compare One _ = LT
  compare Two One = GT
  compare Two Two = EQ
  compare Two Three = LT
  compare Three _ = GT

data Operation = Operation {getOperationValue :: Token, getOperationPriority :: OperationPriority} deriving  Show

data OperationLinkedList = OperationLinkedList {getValue :: Token, getOperation :: Operation, getNextOperation :: OperationLinkedList} | EmptyOperationLinkedList deriving Show

insertOperationByPriority :: OperationLinkedList -> Token -> Operation -> OperationLinkedList
insertOperationByPriority EmptyOperationLinkedList value operation =
  OperationLinkedList value operation EmptyOperationLinkedList
insertOperationByPriority (OperationLinkedList value operation next) newValue newOperation =
  if getOperationPriority operation >= getOperationPriority newOperation
    then OperationLinkedList value operation (insertOperationByPriority next newValue newOperation)
    else OperationLinkedList newValue newOperation (OperationLinkedList value operation next)

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

parse :: [Token] -> OperationLinkedList
parse [] = EmptyOperationLinkedList
parse (x : xs) = case getTokenType x of
  INTEGER -> 
    case getTokenType (head xs) of
      PLUS -> insertOperationByPriority (parse xs) x (Operation (head xs) One)
      MINUS -> insertOperationByPriority (parse xs) x (Operation (head xs) One)
      MULTIPLICATION -> insertOperationByPriority (parse xs) x (Operation (head xs) Two)
      DIVISION -> insertOperationByPriority (parse xs) x (Operation (head xs) Two)
      _ -> insertOperationByPriority EmptyOperationLinkedList x (Operation (head xs) One)
  _ -> parse xs

sampleString :: String
sampleString = "7 * 3 + 2 - 1"

myList :: [Token]
myList = validateSyntax $ tokenize sampleString