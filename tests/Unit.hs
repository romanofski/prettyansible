{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Parser

import Data.Attoparsec.Text (parseOnly)
import Test.Tasty (defaultMain, TestTree, TestName, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


tests :: TestTree
tests = testGroup "unit tests" [ testParsesTasks , testParseTaskOutput]

testParsesTasks :: TestTree
testParsesTasks =
    testGroup "parses tasks correctly" $
    (\(n,i,e) ->
          testCase n $ i @?= e) <$>
    [ ( "with timestamp"
      , parseOnly
            Parser.parseTask
            "TASK [setup] ***\nThursday 56:13 +1000 (0:00:00.087)       0:00:01.678 ***** \nok: [lab-host.com]\nok: [foobar.com]"
      , (Right $
         Parser.Task
         { Parser.name = "TASK [setup] ***\nThursday 56:13 +1000 (0:00:00.087)       0:00:01.678 ***** "
         , Parser.taskoutput = [ Parser.TaskOutput Parser.OK "lab-host.com" ""
                               , Parser.TaskOutput Parser.OK "foobar.com" ""]
         }))]

testParseTaskOutput :: TestTree
testParseTaskOutput =
    testGroup "parses task output correctly" $
    (\(n,i,e) ->
          testCase n $ i @?= e) <$>
    [ ( "normal hosts"
      , parseOnly Parser.parseTaskOutput "ok: [lab-host.com]\n"
      , (Right $ Parser.TaskOutput Parser.OK "lab-host.com" ""))
    , ( "with output"
      , parseOnly
            Parser.parseTaskOutput
            "ok: [lab-host.com] => (item=u'foobar')\n"
      , (Right $ Parser.TaskOutput Parser.OK "lab-host.com" "(item=u'foobar')"))]

main :: IO ()
main = defaultMain tests
