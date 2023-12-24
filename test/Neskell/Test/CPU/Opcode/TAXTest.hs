module Neskell.Test.CPU.Opcode.TAXTest (tests) where

import Neskell.CPU (CPU, cpuRegister)
import Neskell.CPU.Opcode.TAX (implied)
import Neskell.CPU.Register (Register (regX), regPS)
import Neskell.CPU.Register.ProcessorStatus (ProcessorStatus (sN, sZ))
import Neskell.Test.Arbitrary
import Neskell.Test.Util (setRegA)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "TAX"
    [prop_implied]

prop_implied :: TestTree
prop_implied =
  testGroup
    "immediate"
    [ testGroup
        "Zero flag"
        [ testProperty "Copied value is 0, then it causes setting Zero flag"
            $ \cpu -> zeroFlagTest (setRegA 0 (cpu :: CPU)) == Right True
        , testProperty "Copied value is non 0, then it cause unset Zero flag"
            $ \cpu (NonZeroRegisterValue v) -> zeroFlagTest (setRegA v (cpu :: CPU)) == Right False
        ]
    , testGroup
        "Negative flag"
        [ testProperty "Copied value is bit 7 set number, then it causes Negative flag"
            $ \cpu (Bit7SetRegisterValue v) -> negativeFlagTest (setRegA v (cpu :: CPU)) == Right True
        , testProperty "Copied value is not bit 7 set number, then it cause unset Negative flag"
            $ \cpu (Bit7UnsetRegisterValue v) -> negativeFlagTest (setRegA v (cpu :: CPU)) == Right False
        ]
    , testProperty "Copy accumulator value to X register"
        $ \cpu (RegisterValue v) -> fmap (regX . cpuRegister) (implied (setRegA v (cpu :: CPU))) == Right v
    ]
 where
  zeroFlagTest cpu = fmap (sZ . regPS . cpuRegister) (implied cpu)
  negativeFlagTest cpu = fmap (sN . regPS . cpuRegister) (implied cpu)
