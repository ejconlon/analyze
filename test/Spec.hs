{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Analyze.Conversions as AC
import           Analyze.CFrame as ACF
import           Analyze.RFrame as ARF
import           Control.Exception (catch)
import           Fixtures
import           Generation
import           Test.QuickCheck
import           Test.QuickCheck.Property as P
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

-- Boilerplate

instance Testable Assertion where
  property = propertyIO
  exhaustive _ = True

propertyIO :: Assertion -> Property
propertyIO action = ioProperty tester
  where
    tester :: IO P.Result
    tester = catch (action >> return P.succeeded) handler
    handler (HUnitFailure err) = return P.failed { P.reason = err }

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

-- testGen :: TestTree
-- testGen = testProperty "gen" $ \prop -> do
--   1 @?= 1

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testFixture
  --, testGen
  ]

main :: IO ()
main = defaultMain tests
