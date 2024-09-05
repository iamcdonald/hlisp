module LispTest where

import Data.Either
import Lisp qualified as Lisp
import Parser (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Types (LispEvaluatorException (..), LispExpr (..), LispOp (..), LispParserException (..))

suite :: TestTree
suite =
  testGroup
    "Lisp"
    [ testCase "lisp - simple - (+ 2 3)" simple,
      testCase "lisp - nested - (second (list 1 (+ 2 4) 9))" nested,
      testCase "eval exception - unsupported operator  - (+ 2 4)" unsupportedOperator,
      testCase "eval exception - unsupported operatition use  - (+ 2 4)" unsupportedOperatorUse,
      testCase "parse exception - unmatched - ((+ 2 4)" unmatchedProg,
      testCase "parse exception - incomplete - + 2 4)" incompleteProg
    ]
  where
    opwrapper op (LispInteger x) (LispInteger y) = LispInteger (x `op` y)
    opwrapper _ _ _ = LispNothing
    arith :: (Integer -> Integer -> Integer) -> [LispExpr] -> LispExpr
    arith op = foldl1 (opwrapper op)
    second (LispList (_ : x : _) : _) = x
    second _ = LispNothing
    ops =
      [ LispOp {op = "+", fn = (arith (+)), usage = "Woah there! plus"},
        LispOp {op = "list", fn = LispList, usage = "Woah there! list"},
        LispOp {op = "second", fn = second, usage = "Woah there! second"}
      ]
    lisp = Lisp.create ops
    simple :: Assertion
    simple =
      assertEqual "" (Lisp.Result (Just $ LispInteger 5) Nothing) $ lisp "(+ 3 2)"
    nested :: Assertion
    nested =
      assertEqual "" (Lisp.Result (Just $ LispInteger 6) Nothing) $ lisp "(second (list 1 (+ 2 4) 9))"
    unsupportedOperator :: Assertion
    unsupportedOperator =
      assertEqual "" (Lisp.Result (Just parsed) $ Just $ Right [UnsupportedOperator "-"]) $ lisp prog
      where
        prog = "(- 2 4)"
        parsed = fromLeft (LispList []) $ parse prog
    unsupportedOperatorUse :: Assertion
    unsupportedOperatorUse =
      assertEqual "" (Lisp.Result (Just parsed) $ Just $ Right [UnsupportedOperatorUse "+ - Woah there! plus"]) $ lisp prog
      where
        prog = "(+ 2 \"hello\")"
        parsed = fromLeft (LispList []) $ parse prog
    unmatchedProg :: Assertion
    unmatchedProg =
      assertEqual "" (Lisp.Result Nothing $ Just $ Left $ UnmatchedDelimiter "((+ 2 4)") $ lisp "((+ 2 4)"
    incompleteProg :: Assertion
    incompleteProg =
      assertEqual "" (Lisp.Result Nothing $ Just $ Left $ IncompleteExpression "Does not begin with '('") $ lisp "+ 2 4)"
