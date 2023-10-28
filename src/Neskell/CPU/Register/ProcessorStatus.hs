module Neskell.CPU.Register.ProcessorStatus where

data ProcessorStatus = ProcessorStatus
  { sC :: Bool,
    sZ :: Bool,
    sI :: Bool,
    sD :: Bool,
    sV :: Bool,
    sN :: Bool
  }

processorStatus :: ProcessorStatus
processorStatus = ProcessorStatus False False False False False False

onC :: ProcessorStatus -> ProcessorStatus
onC (ProcessorStatus _ z i d v n) = ProcessorStatus True z i d v n

offC :: ProcessorStatus -> ProcessorStatus
offC (ProcessorStatus _ z i d v n) = ProcessorStatus False z i d v n
