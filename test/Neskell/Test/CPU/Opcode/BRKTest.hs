module Neskell.Test.CPU.Opcode.BRKTest (tests) where

import Neskell.CPU (CPU)
import Neskell.CPU.Opcode.BRK (implied)
import Neskell.CPU.Register.ProcessorStatus (ProcessorStatus (sB))
import Neskell.Test.Arbitrary ()
import Neskell.Test.Util (ps)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "BRK"
    [ prop_implied
    ]

prop_implied :: TestTree
prop_implied =
  testGroup
    "implied"
    [ testProperty "Break flag"
        $ \cpu -> (sB . ps <$> implied (cpu :: CPU)) == Right True
    ]
