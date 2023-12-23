module Neskell.Test.Util where

import Neskell.CPU (CPU (CPU, cpuRegister))
import Neskell.CPU.Register (Register (regPS), updatePS)
import Neskell.CPU.Register.ProcessorStatus (ProcessorStatus)

transPS :: CPU -> (ProcessorStatus -> ProcessorStatus) -> CPU
transPS (CPU r c pc) f = CPU (updatePS r f) c pc

ps :: CPU -> ProcessorStatus
ps = regPS . cpuRegister
