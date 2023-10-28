module Neskell.CPU where

import Neskell.CPU.Register

data CPU = CPU
  { cpuRegister :: Register,
    cpuCycles :: Int,
    cpuSkipCycles :: Int
  }

cpu :: CPU
cpu = CPU register 0 0
