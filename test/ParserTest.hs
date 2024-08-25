module ParserTest where

import Data.Either
import Parser (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Types (LispExpr (..), LispParseException (..))

suite :: TestTree
suite =
  testGroup
    "Parser"
    [ testCase "parse - simple - (+ 2 3)" simple,
      testCase "parse - nested - (first (list 1 (+ 2 3) 9))" nested,
      testCase "exception - incomplete start - + 2 3)" incomplete_beginning,
      testCase "exception - incomplete end - (+ 2 3" incomplete_end,
      testCase "exception - missing delimiter - (+ 2 3()" unmatched_delimiter
    ]
  where
    simple :: Assertion
    simple =
      assertEqual "" (LispList [LispSymbol "+", LispInteger 3, LispInteger 2]) $ fromLeft (LispList []) $ parse "(+ 3 2)"
    nested :: Assertion
    nested =
      assertEqual "" expected $ fromLeft (LispList []) $ parse "(first (list 1 (+ 2 3) 9))"
      where
        expected =
          LispList
            [ LispSymbol "first",
              LispList
                [ LispSymbol "list",
                  LispInteger 1,
                  LispList
                    [ LispSymbol "+",
                      LispInteger 2,
                      LispInteger 3
                    ],
                  LispInteger 9
                ]
            ]
    incomplete_beginning :: Assertion
    incomplete_beginning =
      assertEqual "" (IncompleteExpression "Does not begin with '('") $ fromRight (IncompleteExpression "!") $ parse "+ 2 3)"
    incomplete_end :: Assertion
    incomplete_end =
      assertEqual "" (IncompleteExpression "Does not end with ')'") $ fromRight (IncompleteExpression "!") $ parse "(+ 2 3"
    unmatched_delimiter :: Assertion
    unmatched_delimiter = do
      assertEqual "" (UnmatchedDelimiter "(+ 2 3()") $ fromRight (IncompleteExpression "!") $ parse "(+ 2 3()"
