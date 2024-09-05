module EvaluatorTest where

import Data.Either
import Evaluator (evaluate)
import Parser (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Types (LispEvaluatorException (..), LispExpr (..), LispOp (..))

suite :: TestTree
suite =
  testGroup
    "Evaluator"
    [ testCase "eval - simple - (+ 2 3)" simple,
      testCase "eval - nested - (second (list 1 (+ 2 4) 9))" nested,
      testCase "exception - unsupported operator  - (+ 2 4)" unsupportedOperator,
      testCase "exception - unsupported operatition use  - (+ 2 4)" unsupportedOperatorUse
    ]
  where
    opwrapper op (LispInteger x) (LispInteger y) = LispInteger (x `op` y)
    opwrapper _ _ _ = LispNothing
    arith :: (Integer -> Integer -> Integer) -> [LispExpr] -> LispExpr
    arith op = foldl1 (opwrapper op)

    simple :: Assertion
    simple =
      assertEqual "" (Left (LispInteger 5)) $ evaluate ops $ fromLeft (LispList []) $ parse "(+ 3 2)"
      where
        ops = [LispOp {op = "+", fn = (arith (+)), usage = "Woah there!"}]
    nested :: Assertion
    nested =
      assertEqual "" (Left (LispInteger 6)) $ evaluate ops $ fromLeft (LispList []) $ parse "(second (list 1 (+ 2 4) 9))"
      where
        second (LispList (_ : x : _) : _) = x
        second _ = LispNothing
        ops =
          [ LispOp {op = "+", fn = (arith (+)), usage = "(+) - Woah there!"},
            LispOp {op = "list", fn = LispList, usage = "list - Woah there!"},
            LispOp {op = "second", fn = second, usage = "second - Woah there!"}
          ]
    unsupportedOperator :: Assertion
    unsupportedOperator =
      assertEqual "" (Right [UnsupportedOperator "+"]) $ evaluate [] $ fromLeft (LispList []) $ parse "(+ 2 4)"
    unsupportedOperatorUse :: Assertion
    unsupportedOperatorUse =
      assertEqual "" (Right [UnsupportedOperatorUse "+ - Woah there!, must be numbers"]) $ evaluate ops $ fromLeft (LispList []) $ parse "(+ 2 \"hello\")"
      where
        ops =
          [ LispOp {op = "+", fn = (arith (+)), usage = "Woah there!, must be numbers"}
          ]
