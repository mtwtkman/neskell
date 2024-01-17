module Neskell.Test.CPUTest (tests) where

import Data.Word (Word8)
import Neskell.CPU (CPU (..), readProgram)
import qualified Neskell.Memory as M
import Neskell.Test.Arbitrary ()
import Test.QuickCheck (NonEmptyList (getNonEmpty))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "CPUTest"
    [ prop_readProgram
    ]

prop_readProgram :: TestTree
prop_readProgram =
  testGroup
    "readProgram"
    [ testProperty "reads somes bytes from memory by little endian"
        $ \c@(CPU{}) mem ->
          let
            m = getNonEmpty mem :: [Word8]
            pc = length m - 1
            cpu = c{cpuProgram = Just (M.fromList m), cpuProgramCounter = fromIntegral pc}
           in
            readProgram cpu == Right (m !! pc)
    ]
