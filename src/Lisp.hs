module Lisp (LispExpr, Result (..), create) where

import Evaluator
import Parser
import Types (LispEvaluatorException, LispExpr, LispOp, LispParserException)

data Result = Result {result :: Maybe LispExpr, exception :: Maybe (Either LispParserException [LispEvaluatorException])} deriving (Show, Eq)

create :: [LispOp] -> String -> Result
create ops p = case parse p of
  Right parseEx -> Result Nothing $ Just $ Left parseEx
  Left expr -> case eval expr of
    Right evalEx -> Result (Just expr) $ Just $ Right evalEx
    Left r -> Result (Just r) Nothing
  where
    eval = evaluate ops
