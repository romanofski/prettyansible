module Main where

import Parser

import Data.Attoparsec.Text (parseOnly)
import Test.Tasty (defaultMain, TestTree, TestName, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


tests :: TestTree
tests = testGroup "unit tests" [ testParsesTasks ]

testParsesTasks :: TestTree
testParsesTasks = testGroup "parses tasks correctly" $ \(n, i, e) -> testCase n $ i @?= e <$> [
  ("with timestamp",parseOnly parseTask,(Right $ Task "foo" []))
  ]

main :: IO ()
main = defaultMain tests
