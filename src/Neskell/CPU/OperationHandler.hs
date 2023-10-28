jodule Neskell.CPU.OperationHandler where

import Neskell.CPU (CPU)
import Neskell.CPU.AddressingMode (AddressingMode (..))
import Neskell.CPU.Opcode (Opcode (opAddressingMode))
import qualified Neskell.CPU.OperationHandler.Absolute as AbsoluteHandler
import qualified Neskell.CPU.OperationHandler.AbsoluteX as AbsoluteXHandler
import qualified Neskell.CPU.OperationHandler.AbsoluteY as AbsoluteYHandler
import qualified Neskell.CPU.OperationHandler.Accumulator as AccumulatorHandler
import qualified Neskell.CPU.OperationHandler.Implied as ImpliedHandler
import qualified Neskell.CPU.OperationHandler.Immediate as ImmediateHandler
import qualified Neskell.CPU.OperationHandler.IndexedIndirect as IndexedIndirectHandler
import qualified Neskell.CPU.OperationHandler.Indirect as IndirectHandler
import qualified Neskell.CPU.OperationHandler.IndirectIndexed as IndirectIndexedHandler
import qualified Neskell.CPU.OperationHandler.Relative as RelativeHandler
import qualified Neskell.CPU.OperationHandler.ZeroPage as ZeroPageHandler
import qualified Neskell.CPU.OperationHandler.ZeroPageX as ZeroPageXHandler
import qualified Neskell.CPU.OperationHandler.ZeroPageY as ZeroPageYHandler

handle :: CPU -> Opcode -> CPU
handle cpu op = case (opAddressingMode op) of
  Just Implied -> ImpliedHandler.handle cpu op
  Just Immediate -> ImmediateHandler.handle cpu op
  Just Accumulator -> AccumulatorHandler.handle cpu op
  Just ZeroPage -> ZeroPageHandler.handle cpu op
  Just ZeroPageX -> ZeroPageXHandler.handle cpu op
  Just ZeroPageY -> ZeroPageYHandler.handle cpu op
  Just Relative -> RelativeHandler.handle cpu op
  Just Absolute -> AbsoluteHandler.handle cpu op
  Just AbsoluteX -> AbsoluteXHandler.handle cpu op
  Just AbsoluteY -> AbsoluteYHandler.handle cpu op
  Just Indirect -> IndirectHandler.handle cpu op
  Just IndexedIndirect -> IndexedIndirectHandler.handle cpu op
  Just IndirectIndexed -> IndirectIndexedHandler.handle cpu op
  Nothing -> cpu
