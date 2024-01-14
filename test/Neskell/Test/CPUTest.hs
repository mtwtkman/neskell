module Neskell.Test.CPUTest (tests) where

import Data.Word (Word8)
import Neskell.CPU (CPU (..), readMemory)
import Neskell.Test.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import qualified Neskell.Memory as M

tests :: TestTree
tests =
  testGroup
    "CPUTest"
    [ prop_readMemory
    ]

prop_readMemory :: TestTree
prop_readMemory =
  testGroup
    "readMemory"
    [ testProperty "reads somes bytes from memory by little endian"
        $ \c@(CPU{}) lo hi ->
          let cpu = c{cpuProgram = Just (M.fromList [lo :: Word8, hi :: Word8])}
           in readMemory cpu 0 == Right (hi, lo)
    ]
