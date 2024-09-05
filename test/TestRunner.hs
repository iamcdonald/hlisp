module Main where

import EvaluatorTest
import LispTest
import ParserTest
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "h-lisp"
    [ ParserTest.suite,
      EvaluatorTest.suite,
      LispTest.suite
    ]
