{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Parser

import           Data.Aeson                       (Value (..))
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.HashMap.Lazy                as HM
import           Test.Tasty                       (TestTree, defaultMain,
                                                   testGroup)
import           Test.Tasty.HUnit                 (testCase, (@?=))


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
         , Parser.taskoutput = [ Parser.TaskOutput Parser.OK "lab-host.com" (String "")
                               , Parser.TaskOutput Parser.OK "foobar.com" (String "")]
         }))]

testParseTaskOutput :: TestTree
testParseTaskOutput =
    testGroup "parses task output correctly" $
    (\(n,i,e) ->
          testCase n $ i @?= e) <$>
    [ ( "normal hosts"
      , parseOnly Parser.parseTaskOutput "ok: [lab-host.com]\n"
      , (Right $ Parser.TaskOutput Parser.OK "lab-host.com" (String "")))
    , ( "with output"
      , parseOnly
            Parser.parseTaskOutput
            "ok: [lab-host.com] => {\"changed\": false}\n"
      , (Right $
         Parser.TaskOutput Parser.OK "lab-host.com" $
         Object (HM.singleton "changed" $ Bool False)))]

main :: IO ()
main = defaultMain tests
