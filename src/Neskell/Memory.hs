module Neskell.Memory where

import Data.Word (Word8)
import Data.Vector (Vector)

newtype Memory = Memory (Vector Word8)
  deriving (Show, Eq)

load :: Vector Word8 -> Memory
load = Memory
