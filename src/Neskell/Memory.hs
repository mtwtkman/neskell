module Neskell.Memory where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)

newtype Memory = Memory (Vector Word8)
  deriving (Show, Eq)

load :: Vector Word8 -> Memory
load = Memory

read1 :: Memory -> Int -> Maybe Word8
read1 (Memory mem) pos = mem V.!? pos

read2 :: Memory -> Int -> Maybe (Word8, Word8)
read2 mem pos =
  case (read1 mem pos, read1 mem (pos + 1)) of
    (Just lo, Just hi) -> Just (hi, lo)
    _ -> Nothing

length :: Memory -> Int
length (Memory m) = V.length m

fromList :: [Word8] -> Memory
fromList = load . V.fromList
