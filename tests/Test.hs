import Test.Tasty.HUnit
import Test.Tasty
import Formula
import System.Environment (setEnv)


main :: IO ()
main = do
    setEnv "TASTY_COLOR" "always"
    defaultMain tests


tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [
    testCase "import test" $ foo 10 20 @?= 30
  ]

