module Neskell.CPU.Register.ProcessorStatus where

data ProcessorStatus = ProcessorStatus
  { sC :: Bool
  , sZ :: Bool
  , sI :: Bool
  , sD :: Bool
  , sB :: Bool
  , sV :: Bool
  , sN :: Bool
  }
  deriving (Show, Eq)

processorStatus :: ProcessorStatus
processorStatus = ProcessorStatus False False False False False False False

onC :: ProcessorStatus -> ProcessorStatus
onC (ProcessorStatus _ z i d b v n) = ProcessorStatus True z i d b v n

offC :: ProcessorStatus -> ProcessorStatus
offC (ProcessorStatus _ z i d b v n) = ProcessorStatus False z i d b v n

onZ :: ProcessorStatus -> ProcessorStatus
onZ (ProcessorStatus c _ i d b v n) = ProcessorStatus c True i d b v n

offZ :: ProcessorStatus -> ProcessorStatus
offZ (ProcessorStatus c _ i d b v n) = ProcessorStatus c False i d b v n

onN :: ProcessorStatus -> ProcessorStatus
onN (ProcessorStatus c z i d b v _) = ProcessorStatus c z i d b v True

offN :: ProcessorStatus -> ProcessorStatus
offN (ProcessorStatus c z i d b v _) = ProcessorStatus c z i d b v False
