{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Analyze.Common ((<&>))
import qualified Analyze.Conversions as AC
import qualified Analyze.Decoding as AD
import           Analyze.DSL
import qualified Analyze.CFrame as ACF
import qualified Analyze.RFrame as ARF
import           Control.Monad.Catch
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Fixtures
import           Generation
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

-- Boilerplate

-- instance Testable Assertion where
--   property = propertyIO
--   exhaustive _ = True

propertyIO :: Assertion -> Property
propertyIO action = ioProperty tester
  where
    tester :: IO P.Result
    tester = catch (action >> return P.succeeded) handler
    handler (HUnitFailure err) = return P.failed { P.reason = err }

testPropertyIO :: TestName -> Gen a -> (a -> Assertion) -> TestTree
testPropertyIO name g t = testProperty name (propertyIO . t <$> g)

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

testGen :: TestTree
testGen = testPropertyIO "gen" valueRFrameGen test
  where
    test rframe =
      let rkeys = ARF.rframeKeys rframe
          rrows = ARF.rframeRows rframe
          rcols = ARF.rframeCols rframe
          cframe = AC.rowToCol rframe
          ckeys = ACF.cframeKeys cframe
          crows = ACF.cframeRows cframe
          ccols = ACF.cframeCols cframe
          rframe' = AC.colToRow cframe
      in do
        ckeys @?= rkeys
        crows @?= rrows
        ccols @?= rcols
        rframe' @?= rframe

testRowDecode :: TestTree
testRowDecode = testCase "rowDecode" $ do
  let decoder = AD.requireWhere "score" floating <&> (*2)
  result <- sequenceA =<< ARF.rframeDecode decoder exampleRFrame
  V.fromList [10.0, 6.0] @?= result

testColDecode :: TestTree
testColDecode = testCase "colDecode" $ do
  1 @?= 1

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testFixture
  --, testGen
  , testRowDecode
  --, testColDecode
  ]

main :: IO ()
main = defaultMain tests
