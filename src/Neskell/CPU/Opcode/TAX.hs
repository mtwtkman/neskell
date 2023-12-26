module Neskell.CPU.Opcode.TAX (implied) where

import Data.Bits (Bits (testBit))
import Neskell.CPU (CPU (CPU))
import Neskell.CPU.Register (Register (Register), forwardProgram, updatePS)
import Neskell.CPU.Register.ProcessorStatus (offN, offZ, onN, onZ)
import Neskell.Type (Operand (Operand0), Result)

zFlag :: Register -> Register
zFlag r@(Register _ _ _ x _ _) = updatePS r (if x == 0 then onZ else offZ)

nFlag :: Register -> Register
nFlag r@(Register _ _ _ x _ _) = updatePS r (if testBit x 7 then onN else offN)

switchFlag :: Register -> Register
switchFlag = zFlag . nFlag

copyA :: Register -> Result Register
copyA (Register pc sp a _ y ps) = Right $ Register pc sp a a y ps

implied :: CPU -> Result CPU
implied (CPU r c pc m) = do
  reg <- copyA $ forwardProgram Operand0 r
  return $ CPU (switchFlag reg) c pc m
