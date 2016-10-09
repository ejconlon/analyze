import           Analyze.Conversions as AC
import           Analyze.CFrame as ACF
import           Analyze.RFrame as ARF
import           Fixtures
import           Test.Tasty
import           Test.Tasty.HUnit

-- Tests

testFixture :: TestTree
testFixture = testCase "fixture" $ do
  (ARF.rframeKeys exampleRFrame) @?= exampleHeader
  (ARF.rframeRows exampleRFrame) @?= 2
  (ARF.rframeCols exampleRFrame) @?= 3
  (ACF.cframeKeys exampleCFrame) @?= exampleHeader
  (ACF.cframeRows exampleCFrame) @?= 2
  (ACF.cframeCols exampleCFrame) @?= 3
  (AC.rowToCol exampleRFrame) @?= exampleCFrame
  (AC.colToRow exampleCFrame) @?= exampleRFrame

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testFixture
  ]

main :: IO ()
main = defaultMain tests
