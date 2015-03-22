{-# LANGUAGE OverloadedStrings #-}

import CMark
import Test.HUnit
import System.Exit
import Data.Text ()

main :: IO ()
main = do
  counts' <- runTestTT tests
  case (errors counts' + failures counts') of
       0 -> exitWith ExitSuccess
       n -> exitWith (ExitFailure n)

tests :: Test
tests = TestList [
    TestCase $ assertEqual "" ("<h1>Hi</h1>\n") (commonmarkToHtml [] "# Hi\n")
  ]

