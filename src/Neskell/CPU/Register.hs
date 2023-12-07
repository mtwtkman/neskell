module Neskell.CPU.Register where

import Data.Word (Word16, Word8)
import Neskell.CPU.Opcode (toProgramSize)
import Neskell.CPU.Register.ProcessorStatus (ProcessorStatus, processorStatus)
import Neskell.Type (Operand)

data Register = Register
  { regPC :: Word16
  , regSP :: Word8
  , regA :: Word8
  , regX :: Word8
  , regY :: Word8
  , regPS :: ProcessorStatus
  }

moveProgramCounter :: (Word16 -> Word16 -> Word16) -> Operand -> Register -> Register
moveProgramCounter direction op (Register pc sp a x y ps) = Register (direction pc (toProgramSize op)) sp a x y ps

forwardProgram :: Operand -> Register -> Register
forwardProgram = moveProgramCounter (+)

backwardProgram :: Operand -> Register -> Register
backwardProgram = moveProgramCounter (-)

register :: Register
register = Register 0 0 0 0 0 processorStatus

loadA :: Word8 -> Register -> Register
loadA n (Register pc sp a x y ps) = Register pc sp (a + n) x y ps
