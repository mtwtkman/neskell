module Neskell.Memory where

import Data.Word (Word16, Word8)

data Memory = Memory
  { memVolume :: Word16
  , memContents :: [Word8]
  }
  deriving (Show, Eq)

memory :: Memory
memory = Memory 0xffff []
