import CMark
import Test.HUnit
import System.Exit

main :: IO ()
main = do
  counts' <- runTestTT tests
  case (errors counts' + failures counts') of
       0 -> exitWith ExitSuccess
       n -> exitWith (ExitFailure n)

tests :: Test
tests = TestList [
    TestCase $ assertEqual "prefix" "1" "1"
  ]

