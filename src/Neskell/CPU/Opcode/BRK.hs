module Neskell.CPU.Opcode.BRK (implied) where

import Neskell.CPU (CPU (CPU))
import Neskell.CPU.Register (forwardProgram, updatePS)
import Neskell.CPU.Register.ProcessorStatus (onB)
import Neskell.Type (Operand (Operand0), Result)

implied :: CPU -> Result CPU
implied (CPU r c pc) = do
  reg <- Right $ forwardProgram Operand0 r
  return (CPU (updatePS reg onB) c pc)
