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
  deriving (Show, Eq)

moveProgramCounter :: (Word16 -> Word16 -> Word16) -> Operand -> Register -> Register
moveProgramCounter direction op reg@(Register{regPC = pc}) = reg{regPC = direction pc (toProgramSize op)}

forwardProgram :: Operand -> Register -> Register
forwardProgram = moveProgramCounter (+)

backwardProgram :: Operand -> Register -> Register
backwardProgram = moveProgramCounter (-)

register :: Register
register = Register 0 0 0 0 0 processorStatus

updatePS :: Register -> (ProcessorStatus -> ProcessorStatus) -> Register
updatePS reg@(Register{regPS = ps}) f = reg{regPS = f ps}

loadA :: Word8 -> Register -> Register
loadA n reg@(Register{}) = reg{regA = n}
