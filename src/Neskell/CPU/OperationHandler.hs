module Neskell.CPU.OperationHandler where

import Neskell.CPU (CPU)
import Neskell.CPU.Opcode (Opcode (opInstruction))
import Neskell.CPU.Instruction (Instruction(Official), Official (ADC))
import qualified Neskell.CPU.Instruction.Arithmetic.ADC as ADCExecutor

handle :: CPU -> Opcode -> CPU
handle cpu op = case opInstruction op of
  Official ADC -> ADCExecutor.execute cpu op
  _ -> cpu
