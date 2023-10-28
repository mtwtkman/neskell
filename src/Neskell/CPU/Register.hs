module Neskell.CPU.Register where

import Data.Word (Word16, Word8)
import Neskell.CPU.Register.ProcessorStatus (ProcessorStatus, processorStatus)

data Register = Register
  { regPC :: Word16,
    regSP :: Word8,
    regA :: Word8,
    regX :: Word8,
    regY :: Word8,
    regPS :: ProcessorStatus
  }

register :: Register
register = Register 0 0 0 0 0 processorStatus
