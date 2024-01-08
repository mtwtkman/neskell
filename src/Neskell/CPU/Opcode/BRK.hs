module Neskell.CPU.Opcode.BRK (implied) where

import Neskell.CPU (CPU (CPU, cpuRegister))
import Neskell.CPU.Register (forwardProgram, updatePS)
import Neskell.CPU.Register.ProcessorStatus (onB)
import Neskell.Type (Operand (Operand0), Result)

implied :: CPU -> Result CPU
implied cpu@(CPU{cpuRegister = reg}) = do
  reg' <- Right $ forwardProgram Operand0 reg
  return cpu{cpuRegister = updatePS reg' onB}
