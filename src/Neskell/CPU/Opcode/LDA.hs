module Neskell.CPU.Opcode.LDA where
import Neskell.CPU (CPU, Program(programOperand, Program))
import Neskell.CPU.Register (loadA)
import Neskell.CPU.Instruction (Official(LDA))
import Neskell.Type (OperandBody1, Result)

immediate :: CPU -> OperandBody1 -> Result CPU
immediate cpu v = do
  undefined
