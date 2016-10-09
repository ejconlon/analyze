import Fixtures
import Test.Tasty
import Test.Tasty.HUnit

-- Tests

testOne :: TestTree
testOne = testCase "one" $ do
  return ()

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testOne
  ]

main :: IO ()
main = defaultMain tests
