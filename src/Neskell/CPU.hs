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
  Error (DecodeError, OpcodeError, ProgramError),
  OpcodeError (UnofficialOpcodeNotSupported),
  OperandBody,
  Program,
  ProgramError (
    ProgramCannotLoad,
    ProgramLengthOverflow,
    ProgramLengthUnderflow,
    ProgramNotLoaded
  ),
  Result,
 )

data CPU = CPU
  { cpuRegister :: Register
  , cpuCycles :: Int
  , cpuPageCrossing :: Bool
  , cpuProgram :: Maybe Program
  , cpuProgramCounter :: Word16
  }
  deriving (Show, Eq)

initialProgramCounter :: Word16
initialProgramCounter = 0x0

cpu :: CPU
cpu = CPU register 0 False Nothing initialProgramCounter

data Operation = Operation
  { programOpCode :: Opcode
  , programOperand :: OperandBody
  }
  deriving (Eq, Show)

setPageCrossed :: CPU -> Word16 -> Word16 -> CPU
setPageCrossed x@(CPU _ _ True _ _) _ _ = x
setPageCrossed x@(CPU{}) a b = x{cpuPageCrossing = a .&. 0xff00 /= b .&. 0xff00}

decodeOpcode :: V.Vector Word8 -> Result Opcode
decodeOpcode program =
  let x = V.head program
   in toOpcode x

readInstruction :: V.Vector Word8 -> Result (Maybe Operation, V.Vector Word8)
readInstruction src =
  if V.null src
    then Right (Nothing, src)
    else do
      case V.uncons src of
        Nothing -> Left $ DecodeError OpcodeNotFound
        Just (h, t) -> do
          op <- toOpcode h
          (rest, operand) <- reifyByte (opBytesSize op) t
          return (Just $ Operation op operand, rest)

process :: CPU -> Operation -> Result CPU
process c p =
  case opInstruction $ programOpCode p of
    Official op -> processOfficial c op (programOperand p)
    Unofficial _ -> Left (OpcodeError UnofficialOpcodeNotSupported)

processOfficial :: CPU -> Official -> OperandBody -> Result CPU
processOfficial = undefined

loadProgram :: CPU -> Program -> Result CPU
loadProgram c p
  | V.length p > 0xffff = Left (ProgramError ProgramLengthOverflow)
  | V.length p == 0 = Left (ProgramError ProgramLengthUnderflow)
  | otherwise = Right c{cpuProgram = Just p}

initialLoad :: Program -> Result CPU
initialLoad = loadProgram cpu

readProgram :: CPU -> Result Word8
readProgram (CPU{cpuProgram = Nothing}) = Left (ProgramError ProgramNotLoaded)
readProgram (CPU{cpuProgram = Just p, cpuProgramCounter = c}) = case p V.!? fromIntegral (toInteger c) of
  Nothing -> Left (ProgramError ProgramCannotLoad)
  Just x -> Right x
