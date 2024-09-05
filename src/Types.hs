module Types where

data LispExpr
  = LispList [LispExpr]
  | LispSymbol String
  | LispInteger Integer
  | LispString String
  | LispNothing
  deriving (Eq)

instance Show LispExpr where
  show (LispInteger i) = show i
  show (LispString s) = ['"'] ++ s ++ ['"']
  show (LispSymbol s) = s
  show (LispList x) = "<" ++ unwords (map show x) ++ ">"
  show (LispNothing) = "null"

data LispParseException = IncompleteExpression String | UnmatchedDelimiter String deriving (Show, Eq)

data LispEvaluatorException = UnsupportedOperator String | UnsupportedOperatorUse String deriving (Show, Eq)

data LispOp = LispOp
  { op :: String,
    fn :: [LispExpr] -> LispExpr,
    usage :: String
  }
