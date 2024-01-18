module Neskell.Test.MemoryTest (tests) where

import Data.Word (Word8)
import Neskell.Memory (read1)
import qualified Neskell.Memory as M
import Neskell.Test.Arbitrary ()
import Test.QuickCheck (NonEmptyList (getNonEmpty))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Data.Maybe (isNothing)

tests :: TestTree
tests =
  testGroup
    "MemoryTest"
    [ prop_read1
    ]

prop_read1 :: TestTree
prop_read1 =
  testGroup
    "read1"
    [ testProperty "should get a value by valid index"
        $ \xs ->
          let mem = M.fromList $ getNonEmpty (xs :: NonEmptyList Word8)
              idx = fromIntegral (M.length mem) - 1
           in read1 mem idx == Just (getNonEmpty xs !! (fromIntegral $ toInteger idx :: Int))
    , testProperty "should Nothing by out of range"
        $ \xs ->
          let mem = M.fromList $ getNonEmpty (xs :: NonEmptyList Word8)
              idx = fromIntegral $ M.length mem
           in isNothing (read1 mem idx)
    ]
