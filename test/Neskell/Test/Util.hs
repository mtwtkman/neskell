module Neskell.Test.Util where

import Neskell.CPU (CPU (CPU))
import Neskell.CPU.Register.ProcessorStatus (ProcessorStatus)
import Neskell.CPU.Register (updatePS)

transPS :: CPU -> (ProcessorStatus -> ProcessorStatus) -> CPU
transPS (CPU r c pc) f = CPU (updatePS r f) c pc
