module Neskell.Test.Util where

import Data.Word (Word8)
import Neskell.CPU (CPU (CPU, cpuRegister))
import Neskell.CPU.Register (
  Register (Register, regA, regPS),
  updatePS,
 )
import Neskell.CPU.Register.ProcessorStatus (ProcessorStatus)

transPS :: CPU -> (ProcessorStatus -> ProcessorStatus) -> CPU
transPS cpu@(CPU{cpuRegister = reg}) f = cpu{cpuRegister = updatePS reg f}

setRegister :: CPU -> (Register -> Register) -> CPU
setRegister cpu@(CPU{cpuRegister = reg}) f = cpu{cpuRegister = f reg}

setRegA :: Word8 -> CPU -> CPU
setRegA v c = setRegister c (\reg@(Register{}) -> reg{regA = v})

ps :: CPU -> ProcessorStatus
ps = regPS . cpuRegister
