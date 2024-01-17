module Neskell.Memory where

import Data.Bits (shiftL, (.|.))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16, Word8)
import Neskell.Type (Error (MemoryError), MemoryError (OutOfRangeMemory), Result)

newtype Memory = Memory (Vector Word8)
  deriving (Show, Eq)

load :: Vector Word8 -> Memory
load = Memory

word16ToInt :: Word16 -> Int
word16ToInt = fromInteger . toInteger

unionBytes :: Word16 -> Word16 -> Word16
unionBytes a b = a `shiftL` 8 .|. b

read1 :: Memory -> Word16 -> Maybe Word8
read1 (Memory mem) = (V.!?) mem . word16ToInt

read2 :: Memory -> Word16 -> Maybe (Word8, Word8)
read2 mem pos =
  case (read1 mem pos, read1 mem (pos + 1)) of
    (Just lo, Just hi) -> Just (hi, lo)
    _ -> Nothing

write1 :: Memory -> Word16 -> Result Memory
write1 (Memory mem) pos
  | word16ToInt pos + 1 > V.length mem = Left $ MemoryError OutOfRangeMemory
  | otherwise =
      case read1 (Memory mem) pos of
        Nothing -> Left $ MemoryError OutOfRangeMemory
        Just v -> Right $ Memory (mem V.// [(word16ToInt pos, v)])

length :: Memory -> Int
length (Memory m) = V.length m

fromList :: [Word8] -> Memory
fromList = load . V.fromList
