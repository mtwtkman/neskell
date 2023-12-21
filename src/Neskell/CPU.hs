module Neskell.CPU where

import Data.Bits ((.&.))
import qualified Data.Vector as V
import Data.Word (Word16, Word8)
import Neskell.CPU.Instruction (Instruction (..), Official)
import Neskell.CPU.Opcode (
  Opcode (opBytesSize, opInstruction),
  reifyByte,
  toOpcode,
 )
import Neskell.CPU.Register (Register, register)
import Neskell.Type (
  DecodeError (OpcodeNotFound),
  Error (DecodeError, OpcodeError),
  OpcodeError (UnofficialOpcodeNotSupported),
  OperandBody,
  Result,
 )

data CPU = CPU
  { cpuRegister :: Register
  , cpuCycles :: Int
  , cpuPageCrossing :: Bool
  }
  deriving (Show, Eq)

cpu :: CPU
cpu = CPU register 0 False

data Program = Program
  { programOpCode :: Opcode
  , programOperand :: OperandBody
  }
  deriving (Eq, Show)

setPageCrossed :: CPU -> Word16 -> Word16 -> CPU
setPageCrossed x@(CPU _ _ True) _ _ = x
setPageCrossed (CPU r c _) a b = CPU r c (a .&. 0xff00 /= b .&. 0xff00)

decodeOpcode :: V.Vector Word8 -> Result Opcode
decodeOpcode program =
  let x = V.head program
   in toOpcode x

readInstruction :: V.Vector Word8 -> Result (Maybe Program, V.Vector Word8)
readInstruction src =
  if V.null src
    then Right (Nothing, src)
    else do
      case V.uncons src of
        Nothing -> Left $ DecodeError OpcodeNotFound
        Just (h, t) -> do
          op <- toOpcode h
          (rest, operand) <- reifyByte (opBytesSize op) t
          return (Just $ Program op operand, rest)

process :: CPU -> Program -> Result CPU
process c p =
  case opInstruction $ programOpCode p of
    Official op -> processOfficial c op (programOperand p)
    Unofficial _ -> Left (OpcodeError UnofficialOpcodeNotSupported)

processOfficial :: CPU -> Official -> OperandBody -> Result CPU
processOfficial = undefined

interpret :: CPU -> V.Vector Word16 -> CPU
interpret c p = undefined
