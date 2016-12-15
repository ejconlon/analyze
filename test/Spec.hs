{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Analyze.Common ((<&>))
import qualified Analyze.Conversions as AC
import qualified Analyze.Decoding as AD
import           Analyze.DSL
import qualified Analyze.RFrame as ARF
import           Analyze.Values
import           Control.Monad.Catch
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
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

-- Aux

getFixture :: Text -> IO (ARF.RFrame Text Value)
getFixture name =
  case HM.lookup name fixtures of
    Just u -> ARF.fromUpdate u
    Nothing -> error ("fixture not found: " ++ (T.unpack name))

-- Tests

testFixture :: TestTree
testFixture = testCase "fixture" $ do
  frame <- getFixture "full"
  (ARF._rframeKeys frame) @?= exampleHeader
  (ARF.rows frame) @?= 2
  (ARF.cols frame) @?= 3

testRowDecode :: TestTree
testRowDecode = testCase "rowDecode" $ do
  frame <- getFixture "full"
  let decoder = AD.requireWhere "score" floating <&> (*2)
  result <- sequenceA =<< ARF.decode decoder frame
  V.fromList [10.0, 6.0] @?= result

testDrop :: TestTree
testDrop = testCase "drop" $ do
  original <- getFixture "full"
  expected <- getFixture "noName"
  (ARF.cols original) @?= 3
  (ARF.cols expected) @?= 2
  let actual = ARF.dropCols (HS.singleton "name") original
  (ARF._rframeKeys actual) @?= (ARF._rframeKeys expected)

testKeep :: TestTree
testKeep = testCase "keep" $ do
  original <- getFixture "full"
  expected <- getFixture "noName"
  (ARF.cols original) @?= 3
  (ARF.cols expected) @?= 2
  let actual = ARF.keepCols (HS.fromList ["id", "score"]) original
  (ARF._rframeKeys actual) @?= (ARF._rframeKeys expected)

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testFixture
  , testRowDecode
  , testDrop
  , testKeep
  ]

main :: IO ()
main = defaultMain tests
