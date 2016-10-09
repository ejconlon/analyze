import Analyze.Conversions as AC
import           Fixtures
import           Test.Tasty
import           Test.Tasty.HUnit

-- Tests

testRowToCol :: TestTree
testRowToCol = testCase "rowToCol" $ do
  (AC.rowToCol exampleRFrame) @?= exampleCFrame

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testRowToCol
  ]

main :: IO ()
main = defaultMain tests
