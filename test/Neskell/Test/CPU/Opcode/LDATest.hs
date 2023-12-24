{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Neskell.Test.CPU.Opcode.LDATest (tests) where

import Neskell.CPU (CPU (cpuRegister))
import Neskell.CPU.Opcode.LDA (immediate)
import Neskell.CPU.Register (Register (regA, regPS))
import Neskell.CPU.Register.ProcessorStatus (
  ProcessorStatus (sN, sZ),
  onN,
  onZ,
 )
import Neskell.Test.Arbitrary
import Neskell.Test.Util (transPS)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "LDA"
    [ prop_immediate
    ]

prop_immediate :: TestTree
prop_immediate =
  testGroup
    "immadiate mode"
    [ testGroup
        "Zero flag"
        [ testProperty "0 value causes setting Zero flag"
            $ \cpu -> zeroFlagTest cpu 0 == Right True
        , testProperty "non-0 value causes unsetting Zero flag"
            $ \cpu (NonZeroRegisterValue v) -> zeroFlagTest (transPS cpu onZ) v == Right False
        ]
    , testGroup
        "Negative flag"
        [ testProperty "set bit 7 causes setting Negative flag"
            $ \cpu (Bit7SetRegisterValue v) -> negativeFlagTest cpu v == Right True
        , testProperty "unset bit 7 causes unsetting Negative flag"
            $ \cpu (Bit7UnsetRegisterValue v) -> negativeFlagTest (transPS cpu onN) v == Right False
        ]
    , testProperty "load value to an accumulator"
        $ \cpu (RegisterValue v) -> fmap (regA . cpuRegister) (immediate (cpu :: CPU) v) == Right v
    ]
 where
  zeroFlagTest cpu v = fmap (sZ . regPS . cpuRegister) (immediate cpu v)
  negativeFlagTest cpu v = fmap (sN . regPS . cpuRegister) (immediate cpu v)
