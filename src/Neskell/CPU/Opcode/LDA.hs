{-# LANGUAGE BinaryLiterals #-}

module Neskell.CPU.Opcode.LDA where

import Data.Bits ((.&.))
import Data.Word (Word8)
import Neskell.CPU (CPU (CPU))
import Neskell.CPU.Register (Register, forwardProgram, loadA, updatePS)
import Neskell.CPU.Register.ProcessorStatus (onN, onZ)
import Neskell.Type (Operand (Operand1), OperandBody1, Result)

immediate :: CPU -> OperandBody1 -> Result CPU
immediate (CPU r c pc) v = do
  reg <- Right $ forwardProgram Operand1 r
  return (CPU (loadA v (flag reg)) c pc)
 where
  whenOnZ :: Register -> Register
  whenOnZ reg = if v == 0 then updatePS reg onZ else reg

  whenOnN :: Register -> Register
  whenOnN reg = if v .&. (0b01000000 :: Word8) > 0 then updatePS reg onN else reg

  flag :: Register -> Register
  flag = whenOnN . whenOnZ
