module Types where

data LispExpr
  = LispList [LispExpr]
  | LispSymbol String
  | LispInteger Integer
  | LispString String
  deriving (Eq)

instance Show LispExpr where
  show (LispInteger i) = show i
  show (LispString s) = ['"'] ++ s ++ ['"']
  show (LispSymbol s) = s
  show (LispList x) = "<" ++ unwords (map show x) ++ ">"

data LispParseException = IncompleteExpression String | UnmatchedDelimiter String deriving (Show, Eq)
