module Neskell.Test.InterpretTest where

import qualified Data.Vector as V
import Neskell.CPU (Program (Program), readInstruction)
import Neskell.CPU.AddressingMode (AddressingMode (..))
import Neskell.CPU.Instruction (
  Instruction (Official),
  Official (..),
 )
import Neskell.CPU.Opcode (Opcode (Opcode))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( testCase, (@?=) )
import Neskell.Type ( Operand(..), OperandBody(..) )

tests :: TestTree
tests =
  testGroup
    "CPU"
    [ test_readInstruction
    ]

test_readInstruction :: TestTree
test_readInstruction =
  testGroup
    "readInstruction"
    [ testCase "0xa9 is interpreted as LDA in immediate mode"
        $ readInstruction (V.fromList [0xa9, 0x05, 0x00])
        @?= Right (Just $ Program (Opcode (Official LDA) Immediate Operand1 2) (OperandBody1 0x05), V.fromList [0x00])
    , testCase "0x00 is interpreted as BRK"
        $ readInstruction (V.fromList [0x00, 0x11])
        @?= Right (Just $ Program (Opcode (Official BRK) Implied Operand0 7) (OperandBody0 ()), V.fromList [0x11])
    ]
