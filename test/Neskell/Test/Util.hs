module Neskell.Test.Util where

import Data.Word (Word8)
import Neskell.CPU (CPU (CPU, cpuRegister))
import Neskell.CPU.Register (
  Register (Register, regPS),
  updatePS,
 )
import Neskell.CPU.Register.ProcessorStatus (ProcessorStatus)

transPS :: CPU -> (ProcessorStatus -> ProcessorStatus) -> CPU
transPS (CPU r c pc m) f = CPU (updatePS r f) c pc m

setRegister :: CPU -> (Register -> Register) -> CPU
setRegister (CPU r c pc m) f = CPU (f r) c pc m

setRegA :: Word8 -> CPU -> CPU
setRegA v c = setRegister c (\(Register pc sp _ x y p) -> Register pc sp v x y p)

ps :: CPU -> ProcessorStatus
ps = regPS . cpuRegister
