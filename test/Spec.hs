{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Analyze.Common ((<&>))
import qualified Analyze.Conversions as AC
import qualified Analyze.Decoding as AD
import           Analyze.DSL
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
  exampleRFrame <- ARF.rframeFromUpdate exampleRFrameUpdate
  (ARF._rframeKeys exampleRFrame) @?= exampleHeader
  (ARF.rframeRows exampleRFrame) @?= 2
  (ARF.rframeCols exampleRFrame) @?= 3

testRowDecode :: TestTree
testRowDecode = testCase "rowDecode" $ do
  exampleRFrame <- ARF.rframeFromUpdate exampleRFrameUpdate
  let decoder = AD.requireWhere "score" floating <&> (*2)
  result <- sequenceA =<< ARF.rframeDecode decoder exampleRFrame
  V.fromList [10.0, 6.0] @?= result

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testFixture
  , testRowDecode
  ]

main :: IO ()
main = defaultMain tests
