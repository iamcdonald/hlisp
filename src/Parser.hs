module Parser (parse) where

import Control.Applicative
import Text.Read (readMaybe)
import Types (LispExpr (..), LispParseException (..))

splitOn :: [Char] -> String -> (String, String)
splitOn c = splt ""
  where
    splt agg [] = (agg, "")
    splt agg (x : xs)
      | x `elem` c = (agg, xs)
      | otherwise = splt (agg ++ [x]) xs

splitOnLast :: [Char] -> String -> (String, String)
splitOnLast c s = do
  let (sr, fr) = splitOn c $ reverse s
  (reverse fr, reverse sr)

-- Input Validation

hasMatchedDelimiters :: String -> Bool
hasMatchedDelimiters s = validate 0 s == (0 :: Integer)
  where
    validate i [] = i
    validate i (x : xs)
      | x == '(' = validate (i + 1) xs
      | x == ')' = validate (i - 1) xs
      | otherwise = validate i xs

validateExpression :: String -> Maybe LispParseException
validateExpression expr =
  case (begins, ends, matchedDelimiters) of
    (False, _, _) -> Just $ IncompleteExpression "Does not begin with '('"
    (_, False, _) -> Just $ IncompleteExpression "Does not end with ')'"
    (_, _, False) -> Just $ UnmatchedDelimiter expr
    _ -> Nothing
  where
    matchedDelimiters = hasMatchedDelimiters expr
    begins = head expr == '('
    ends = last expr == ')'

-- Parsing S-expression

parseSymbol :: String -> LispExpr
parseSymbol = LispSymbol

parseInteger :: String -> Maybe LispExpr
parseInteger s = case (readMaybe s :: Maybe Integer) of
  Nothing -> Nothing
  Just i -> Just $ LispInteger i

parseString :: String -> Maybe LispExpr
parseString [] = Nothing
parseString (x : xs)
  | x == '\"' = Just $ LispString $ init xs
  | otherwise = Nothing

parseValue :: String -> LispExpr
parseValue v = case value of
  Just x -> x
  Nothing -> parseSymbol v
  where
    value = parseInteger v <|> parseString v

parseList :: String -> LispExpr
parseList [] = LispList []
parseList expr = LispList $ _parse [] expr
  where
    _parse l [] = l
    _parse agg (x : xs)
      | x == '(' = do
          let (f, s) = splitOnLast ")" xs
          _parse (agg ++ [parseList f]) s
      | x == ' ' = _parse agg xs
      | otherwise = do
          let (f, s) = splitOn " " (x : xs)
          _parse (agg ++ [parseValue f]) s

parse :: String -> Either LispExpr LispParseException
parse expr = case validation of
  Just err -> Right err
  Nothing -> Left $ parseList $ tail $ init expr
  where
    validation = validateExpression expr
