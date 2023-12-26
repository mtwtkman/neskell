{-# LANGUAGE BinaryLiterals #-}

module Neskell.CPU.Opcode.LDA (immediate) where

import Data.Bits (testBit)
import Data.Word (Word8)
import Neskell.CPU (CPU (CPU))
import Neskell.CPU.Register (Register, forwardProgram, loadA, updatePS)
import Neskell.CPU.Register.ProcessorStatus (offN, offZ, onN, onZ)
import Neskell.Type (Operand (Operand1), OperandBody1, Result)

zFlag :: Word8 -> Register -> Register
zFlag v reg = updatePS reg (if v == 0 then onZ else offZ)

nFlag :: Word8 -> Register -> Register
nFlag v reg = updatePS reg (if testBit v 7 then onN else offN)

switchFlag :: Word8 -> Register -> Register
switchFlag v = nFlag v . zFlag v

immediate :: CPU -> OperandBody1 -> Result CPU
immediate (CPU r c pc m) v = do
  reg <- Right $ forwardProgram Operand1 r
  return (CPU (loadA v (switchFlag v reg)) c pc m)
