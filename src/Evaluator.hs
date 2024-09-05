module Evaluator (evaluate) where

import Data.Either
import Data.Map qualified as Map
import Types (LispEvaluatorException (..), LispExpr (..), LispOp (..), LispResult (..))

type LispOps = Map.Map String LispOp

evaluate :: [LispOp] -> LispExpr -> LispResult
evaluate oplist expr =
  eval ops expr
  where
    ops = Map.fromList $ map (\lo@LispOp{op} -> (op, lo)) oplist

eval :: LispOps -> LispExpr -> LispResult
eval ops ll@(LispList((LispSymbol sym) : xs)) =
  case (Map.lookup sym ops) of
    Just op -> withOp op xs
    Nothing -> LispResult ll [UnsupportedOperator sym]
  where
    partition :: LispResult -> Either LispExpr [LispEvaluatorException]
    partition LispResult{result, exceptions = []} = Left result
    partition LispResult{exceptions} = Right exceptions
    withOp :: LispOp -> [LispExpr] -> LispResult
    withOp LispOp{fn,usage} args
      | length exceptions > 0 = LispResult ll exceptions
      | otherwise = case (fn expr) of
          LispNothing -> LispResult ll [UnsupportedOperatorUse $ sym ++ " - " ++ usage]
          value -> LispResult value []
      where
        (expr,exc) = partitionEithers $ map (partition . eval ops) args
        exceptions = concat exc
eval _ expr = LispResult expr []
