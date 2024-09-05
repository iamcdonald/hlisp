module Evaluator (evaluate) where

import Data.Either
import Data.Map qualified as Map
import Types (LispEvaluatorException (..), LispExpr (..), LispOp (..))

type LispOps = Map.Map String LispOp

evaluate :: [LispOp] -> LispExpr -> Either LispExpr [LispEvaluatorException]
evaluate oplist expr =
  eval ops expr
  where
    ops = Map.fromList $ map (\lo@LispOp{op} -> (op, lo)) oplist

eval :: LispOps -> LispExpr -> Either LispExpr [LispEvaluatorException]
eval ops (LispList((LispSymbol sym) : xs)) =
  case (Map.lookup sym ops) of
    Just op -> withOp op xs
    Nothing -> Right [UnsupportedOperator sym]
  where
    withOp :: LispOp -> [LispExpr] -> Either LispExpr [LispEvaluatorException]
    withOp LispOp{fn,usage} args
      | length exceptions > 0 = Right exceptions
      | otherwise = case (fn expr) of
          LispNothing -> Right [UnsupportedOperatorUse $ sym ++ " - " ++ usage]
          value -> Left value
      where
        (expr,exc) = partitionEithers $ map (eval ops) args
        exceptions = concat exc
eval _ expr = Left expr
