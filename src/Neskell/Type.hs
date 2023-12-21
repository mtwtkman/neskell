module Neskell.Type where

import Data.Word (Word8)

type Result a = Either Error a

data OpcodeError
  = UnknownOpcode
  | UnofficialOpcodeNotSupported
  deriving (Show, Eq)

data DecodeError
  = UnmatchedByteSize
  | OpcodeNotFound
  deriving (Show, Eq)

data Error
  = OpcodeError OpcodeError
  | DecodeError DecodeError
  deriving (Show, Eq)

data Operand
  = Operand0
  | Operand1
  | Operand2
  deriving (Show, Eq)

type OperandBody0 = ()
type OperandBody1 = Word8
type OperandBody2 = (Word8, Word8)

data OperandBody
  = OperandBody0 OperandBody0
  | OperandBody1 OperandBody1
  | OperandBody2 OperandBody2
  deriving (Show, Eq)

toProgramSize :: (Num n) => Operand -> n
toProgramSize Operand0 = 1
toProgramSize Operand1 = 2
toProgramSize Operand2 = 3
