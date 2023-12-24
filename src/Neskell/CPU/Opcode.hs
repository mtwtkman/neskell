module Neskell.CPU.Opcode where

import qualified Data.Vector as V
import Data.Word (Word8, Word16)
import Neskell.CPU.AddressingMode (AddressingMode (..))
import Neskell.CPU.Instruction (Instruction (..), Official (..))
import Neskell.Type (
  DecodeError (..),
  Error (..),
  OpcodeError (UnknownOpcode),
  Operand (..),
  OperandBody (..),
  Result,
 )

pickByte :: V.Vector Word8 -> Result (Word8, V.Vector Word8)
pickByte v = case V.uncons v of
  Nothing -> Left $ DecodeError UnmatchedByteSize
  Just xs -> Right xs

reifyByte :: Operand -> V.Vector Word8 -> Result (V.Vector Word8, OperandBody)
reifyByte Operand0 v = Right (v, OperandBody0 ())
reifyByte Operand1 v = do
  (a, rest) <- pickByte v
  return (rest, OperandBody1 a)
reifyByte Operand2 v = do
  (a, rest) <- pickByte v
  (b, rest') <- pickByte rest
  return (rest', OperandBody2 (a, b))

toProgramSize :: Operand -> Word16
toProgramSize Operand0 = 1
toProgramSize Operand1 = 2
toProgramSize Operand2 = 3

data Opcode = Opcode
  { opInstruction :: Instruction
  , opAddressingMode :: AddressingMode
  , opBytesSize :: Operand
  , opCycle :: Int
  }
  deriving (Eq, Show)

toOpcode :: Word8 -> Result Opcode
-- ADC: http://6502.org/users/obelisk/6502/reference.html#ADC
toOpcode 0x69 = Right $ Opcode (Official ADC) Immediate Operand1 2
toOpcode 0x65 = Right $ Opcode (Official ADC) ZeroPage Operand1 3
toOpcode 0x75 = Right $ Opcode (Official ADC) ZeroPageX Operand1 4
toOpcode 0x6d = Right $ Opcode (Official ADC) Absolute Operand2 4
toOpcode 0x7d = Right $ Opcode (Official ADC) AbsoluteX Operand2 4
toOpcode 0x79 = Right $ Opcode (Official ADC) AbsoluteY Operand2 4
toOpcode 0x61 = Right $ Opcode (Official ADC) IndexedIndirect Operand1 6
toOpcode 0x71 = Right $ Opcode (Official ADC) IndirectIndexed Operand1 5
-- AND: http://6502.org/users/obelisk/6502/reference.html#AND
toOpcode 0x29 = Right $ Opcode (Official AND) Immediate Operand1 2
toOpcode 0x25 = Right $ Opcode (Official AND) ZeroPage Operand1 3
toOpcode 0x35 = Right $ Opcode (Official AND) ZeroPageX Operand1 4
toOpcode 0x2d = Right $ Opcode (Official AND) Absolute Operand2 4
toOpcode 0x3d = Right $ Opcode (Official AND) AbsoluteX Operand2 4
toOpcode 0x39 = Right $ Opcode (Official AND) AbsoluteY Operand2 4
toOpcode 0x21 = Right $ Opcode (Official AND) IndexedIndirect Operand1 6
toOpcode 0x31 = Right $ Opcode (Official AND) IndirectIndexed Operand1 5
-- ASL: http://6502.org/users/obelisk/6502/reference.html#ASL
toOpcode 0x0a = Right $ Opcode (Official ASL) Accumulator Operand0 2
toOpcode 0x06 = Right $ Opcode (Official ASL) ZeroPage Operand1 5
toOpcode 0x16 = Right $ Opcode (Official ASL) ZeroPageX Operand1 6
toOpcode 0x0e = Right $ Opcode (Official ASL) Absolute Operand2 6
toOpcode 0x1e = Right $ Opcode (Official ASL) AbsoluteX Operand2 7
-- BCC: http://6502.org/users/obelisk/6502/reference.html#BCC
toOpcode 0x90 = Right $ Opcode (Official BCC) Relative Operand1 2
-- BCS: http://6502.org/users/obelisk/6502/reference.html#BCS
toOpcode 0xb0 = Right $ Opcode (Official BCS) Relative Operand1 2
-- BEQ: http://6502.org/users/obelisk/6502/reference.html#BEQ
toOpcode 0xf0 = Right $ Opcode (Official BEQ) Relative Operand1 2
-- BIT: http://6502.org/users/obelisk/6502/reference.html#BIT
toOpcode 0x24 = Right $ Opcode (Official BIT) ZeroPage Operand1 3
toOpcode 0x2c = Right $ Opcode (Official BIT) Absolute Operand2 4
-- BMI: http://6502.org/users/obelisk/6502/reference.html#BMI
toOpcode 0x30 = Right $ Opcode (Official BMI) Relative Operand1 2
-- BNE: http://6502.org/users/obelisk/6502/reference.html#BNE
toOpcode 0xd0 = Right $ Opcode (Official BNE) Relative Operand1 2
-- BPL: http://6502.org/users/obelisk/6502/reference.html#BPL
toOpcode 0x10 = Right $ Opcode (Official BPL) Relative Operand1 2
-- BRK: http://6502.org/users/obelisk/6502/reference.html#BRK
toOpcode 0x00 = Right $ Opcode (Official BRK) Implied Operand0 7
-- BVC: http://6502.org/users/obelisk/6502/reference.html#BVC
toOpcode 0x50 = Right $ Opcode (Official BVC) Relative Operand1 2
-- BVS: http://6502.org/users/obelisk/6502/reference.html#BVS
toOpcode 0x70 = Right $ Opcode (Official BVS) Relative Operand1 2
-- CLC: http://6502.org/users/obelisk/6502/reference.html#CLC
toOpcode 0x18 = Right $ Opcode (Official CLC) Implied Operand0 2
-- CLD: http://6502.org/users/obelisk/6502/reference.html#CLD
toOpcode 0xd8 = Right $ Opcode (Official CLD) Implied Operand0 2
-- CLI: http://6502.org/users/obelisk/6502/reference.html#CLI
toOpcode 0x58 = Right $ Opcode (Official CLI) Implied Operand0 2
-- CLV: http://6502.org/users/obelisk/6502/reference.html#CLV
toOpcode 0xb8 = Right $ Opcode (Official CLV) Implied Operand0 2
-- CMP: http://6502.org/users/obelisk/6502/reference.html#CMP
toOpcode 0xc9 = Right $ Opcode (Official CMP) Immediate Operand1 2
toOpcode 0xc5 = Right $ Opcode (Official CMP) ZeroPage Operand1 3
toOpcode 0xd5 = Right $ Opcode (Official CMP) ZeroPageX Operand1 4
toOpcode 0xcd = Right $ Opcode (Official CMP) Absolute Operand2 4
toOpcode 0xdd = Right $ Opcode (Official CMP) AbsoluteX Operand2 4
toOpcode 0xd9 = Right $ Opcode (Official CMP) AbsoluteY Operand2 4
toOpcode 0xc1 = Right $ Opcode (Official CMP) IndexedIndirect Operand1 6
toOpcode 0xd1 = Right $ Opcode (Official CMP) IndirectIndexed Operand1 5
-- CPX: http://6502.org/users/obelisk/6502/reference.html#CPX
toOpcode 0xe0 = Right $ Opcode (Official CPX) Immediate Operand1 2
toOpcode 0xe4 = Right $ Opcode (Official CPX) ZeroPage Operand1 3
toOpcode 0xec = Right $ Opcode (Official CPX) Absolute Operand2 4
-- CPY: http://6502.org/users/obelisk/6502/reference.html#CPY
toOpcode 0xc0 = Right $ Opcode (Official CPY) Immediate Operand1 2
toOpcode 0xc4 = Right $ Opcode (Official CPY) ZeroPage Operand1 3
toOpcode 0xcc = Right $ Opcode (Official CPY) Absolute Operand2 4
-- DEC: http://6502.org/users/obelisk/6502/reference.html#DEC
toOpcode 0xc6 = Right $ Opcode (Official DEC) ZeroPage Operand1 5
toOpcode 0xd6 = Right $ Opcode (Official DEC) ZeroPageX Operand1 6
toOpcode 0xce = Right $ Opcode (Official DEC) Absolute Operand2 6
toOpcode 0xde = Right $ Opcode (Official DEC) AbsoluteX Operand2 7
-- DEX: http://6502.org/users/obelisk/6502/reference.html#DEX
toOpcode 0xca = Right $ Opcode (Official DEC) Implied Operand0 2
-- DEY: http://6502.org/users/obelisk/6502/reference.html#DEY
toOpcode 0x88 = Right $ Opcode (Official DEC) Implied Operand0 2
-- EOR: http://6502.org/users/obelisk/6502/reference.html#EOR
toOpcode 0x49 = Right $ Opcode (Official EOR) Immediate Operand1 2
toOpcode 0x45 = Right $ Opcode (Official EOR) ZeroPage Operand1 3
toOpcode 0x55 = Right $ Opcode (Official EOR) ZeroPageX Operand1 4
toOpcode 0x4d = Right $ Opcode (Official EOR) Absolute Operand2 4
toOpcode 0x5d = Right $ Opcode (Official EOR) AbsoluteX Operand2 4
toOpcode 0x59 = Right $ Opcode (Official EOR) AbsoluteY Operand2 4
toOpcode 0x41 = Right $ Opcode (Official EOR) IndexedIndirect Operand1 6
toOpcode 0x51 = Right $ Opcode (Official EOR) IndirectIndexed Operand1 5
-- INC: http://6502.org/users/obelisk/6502/reference.html#INC
toOpcode 0xe6 = Right $ Opcode (Official INC) ZeroPage Operand1 5
toOpcode 0xf6 = Right $ Opcode (Official INC) ZeroPageX Operand1 6
toOpcode 0xee = Right $ Opcode (Official INC) Absolute Operand2 6
toOpcode 0xfe = Right $ Opcode (Official INC) AbsoluteX Operand2 7
-- INX: http://6502.org/users/obelisk/6502/reference.html#INX
toOpcode 0xe8 = Right $ Opcode (Official INX) Implied Operand0 2
-- INY: http://6502.org/users/obelisk/6502/reference.html#INY
toOpcode 0xc8 = Right $ Opcode (Official INY) Implied Operand0 2
-- JMP: http://6502.org/users/obelisk/6502/reference.html#JMP
toOpcode 0x4c = Right $ Opcode (Official JMP) Absolute Operand2 3
toOpcode 0x6c = Right $ Opcode (Official JMP) Indirect Operand2 5
-- JSR: http://6502.org/users/obelisk/6502/reference.html#JSR
toOpcode 0x20 = Right $ Opcode (Official JSR) Absolute Operand2 6
-- LDA: http://6502.org/users/obelisk/6502/reference.html#LDA
toOpcode 0xa9 = Right $ Opcode (Official LDA) Immediate Operand1 2
toOpcode 0xa5 = Right $ Opcode (Official LDA) ZeroPage Operand1 3
toOpcode 0xb5 = Right $ Opcode (Official LDA) ZeroPageX Operand1 4
toOpcode 0xad = Right $ Opcode (Official LDA) Absolute Operand2 4
toOpcode 0xbd = Right $ Opcode (Official LDA) AbsoluteX Operand2 4
toOpcode 0xb9 = Right $ Opcode (Official LDA) AbsoluteY Operand2 4
toOpcode 0xa1 = Right $ Opcode (Official LDA) IndexedIndirect Operand1 6
toOpcode 0xb1 = Right $ Opcode (Official LDA) IndirectIndexed Operand1 5
-- LDX: http://6502.org/users/obelisk/6502/reference.html#LDX
toOpcode 0xa2 = Right $ Opcode (Official LDX) Immediate Operand1 2
toOpcode 0xa6 = Right $ Opcode (Official LDX) ZeroPage Operand1 3
toOpcode 0xb6 = Right $ Opcode (Official LDX) ZeroPageY Operand1 4
toOpcode 0xae = Right $ Opcode (Official LDX) Absolute Operand2 4
toOpcode 0xbe = Right $ Opcode (Official LDX) AbsoluteY Operand2 4
-- LDY: http://6502.org/users/obelisk/6502/reference.html#LDY
toOpcode 0xa0 = Right $ Opcode (Official LDY) Immediate Operand1 2
toOpcode 0xa4 = Right $ Opcode (Official LDY) ZeroPage Operand1 3
toOpcode 0xb4 = Right $ Opcode (Official LDY) ZeroPageX Operand1 4
toOpcode 0xac = Right $ Opcode (Official LDY) Absolute Operand2 4
toOpcode 0xbc = Right $ Opcode (Official LDY) AbsoluteX Operand2 4
-- LSR: http://6502.org/users/obelisk/6502/reference.html#LSR
toOpcode 0x4a = Right $ Opcode (Official LSR) Accumulator Operand0 2
toOpcode 0x46 = Right $ Opcode (Official LSR) ZeroPage Operand1 5
toOpcode 0x56 = Right $ Opcode (Official LSR) ZeroPageX Operand1 6
toOpcode 0x4e = Right $ Opcode (Official LSR) Absolute Operand2 6
toOpcode 0x5e = Right $ Opcode (Official LSR) AbsoluteX Operand2 7
-- NOP: http://6502.org/users/obelisk/6502/reference.html#NOP
toOpcode 0xea = Right $ Opcode (Official NOP) Implied Operand0 2
-- ORA: http://6502.org/users/obelisk/6502/reference.html#ORA
toOpcode 0x09 = Right $ Opcode (Official ORA) Immediate Operand1 2
toOpcode 0x05 = Right $ Opcode (Official ORA) ZeroPage Operand1 3
toOpcode 0x15 = Right $ Opcode (Official ORA) ZeroPageX Operand1 4
toOpcode 0x0d = Right $ Opcode (Official ORA) Absolute Operand2 4
toOpcode 0x1d = Right $ Opcode (Official ORA) AbsoluteX Operand2 4
toOpcode 0x19 = Right $ Opcode (Official ORA) AbsoluteY Operand2 4
toOpcode 0x01 = Right $ Opcode (Official ORA) IndexedIndirect Operand1 6
toOpcode 0x11 = Right $ Opcode (Official ORA) IndirectIndexed Operand1 5
-- PHA: http://6502.org/users/obelisk/6502/reference.html#PHA
toOpcode 0x48 = Right $ Opcode (Official PHA) Implied Operand0 3
-- PHP: http://6502.org/users/obelisk/6502/reference.html#PHP
toOpcode 0x68 = Right $ Opcode (Official PHP) Implied Operand0 4
-- PLP: http://6502.org/users/obelisk/6502/reference.html#PLP
toOpcode 0x28 = Right $ Opcode (Official PLP) Implied Operand0 4
-- ROL: http://6502.org/users/obelisk/6502/reference.html#ROL
toOpcode 0x2a = Right $ Opcode (Official ROL) Accumulator Operand0 2
toOpcode 0x26 = Right $ Opcode (Official ROL) ZeroPage Operand1 5
toOpcode 0x36 = Right $ Opcode (Official ROL) ZeroPageX Operand1 6
toOpcode 0x2e = Right $ Opcode (Official ROL) Absolute Operand2 6
toOpcode 0x3e = Right $ Opcode (Official ROL) AbsoluteX Operand2 7
-- RO$: http://6502.org/users/obelisk/6502/reference.html#ROR
toOpcode 0x6a = Right $ Opcode (Official ROR) Accumulator Operand0 2
toOpcode 0x66 = Right $ Opcode (Official ROR) ZeroPage Operand1 5
toOpcode 0x76 = Right $ Opcode (Official ROR) ZeroPageX Operand1 6
toOpcode 0x6e = Right $ Opcode (Official ROR) Absolute Operand2 6
toOpcode 0x7e = Right $ Opcode (Official ROR) AbsoluteX Operand2 7
-- RTI: http://6502.org/users/obelisk/6502/reference.html#RTI
toOpcode 0x40 = Right $ Opcode (Official RTI) Implied Operand0 6
-- RTS: http://6502.org/users/obelisk/6502/reference.html#RTS
toOpcode 0x60 = Right $ Opcode (Official RTS) Implied Operand0 6
-- SBC: http://6502.org/users/obelisk/6502/reference.html#SBC
toOpcode 0xe9 = Right $ Opcode (Official SBC) Immediate Operand1 2
toOpcode 0xe5 = Right $ Opcode (Official SBC) ZeroPage Operand1 3
toOpcode 0xf5 = Right $ Opcode (Official SBC) ZeroPageX Operand1 4
toOpcode 0xed = Right $ Opcode (Official SBC) Absolute Operand2 4
toOpcode 0xfd = Right $ Opcode (Official SBC) AbsoluteX Operand2 4
toOpcode 0xf9 = Right $ Opcode (Official SBC) AbsoluteY Operand2 4
toOpcode 0xe1 = Right $ Opcode (Official SBC) IndexedIndirect Operand1 6
toOpcode 0xf1 = Right $ Opcode (Official SBC) IndirectIndexed Operand1 5
-- SEC: http://6502.org/users/obelisk/6502/reference.html#SEC
toOpcode 0x38 = Right $ Opcode (Official SEC) Implied Operand0 2
-- SED: http://6502.org/users/obelisk/6502/reference.html#SED
toOpcode 0xf8 = Right $ Opcode (Official SED) Implied Operand0 2
-- SEI: http://6502.org/users/obelisk/6502/reference.html#SEI
toOpcode 0x78 = Right $ Opcode (Official SEI) Implied Operand0 2
-- STA: http://6502.org/users/obelisk/6502/reference.html#STA
toOpcode 0x85 = Right $ Opcode (Official STA) ZeroPage Operand1 3
toOpcode 0x95 = Right $ Opcode (Official STA) ZeroPageX Operand1 4
toOpcode 0x8d = Right $ Opcode (Official STA) Absolute Operand2 4
toOpcode 0x9d = Right $ Opcode (Official STA) AbsoluteX Operand2 5
toOpcode 0x99 = Right $ Opcode (Official STA) AbsoluteY Operand2 5
toOpcode 0x81 = Right $ Opcode (Official STA) IndexedIndirect Operand1 6
toOpcode 0x91 = Right $ Opcode (Official STA) IndirectIndexed Operand1 6
-- STX: http://6502.org/users/obelisk/6502/reference.html#STX
toOpcode 0x86 = Right $ Opcode (Official STX) ZeroPage Operand1 3
toOpcode 0x96 = Right $ Opcode (Official STX) ZeroPageY Operand1 4
toOpcode 0x8e = Right $ Opcode (Official STX) Absolute Operand2 4
-- STY: http://6502.org/users/obelisk/6502/reference.html#STY
toOpcode 0x84 = Right $ Opcode (Official STY) ZeroPage Operand1 3
toOpcode 0x94 = Right $ Opcode (Official STY) ZeroPageX Operand1 4
toOpcode 0x8c = Right $ Opcode (Official STY) Absolute Operand2 4
-- TAX: http://6502.org/users/obelisk/6502/reference.html#TAX
toOpcode 0xaa = Right $ Opcode (Official TAX) Implied Operand0 2
-- TAY: http://6502.org/users/obelisk/6502/reference.html#TAY
toOpcode 0xa8 = Right $ Opcode (Official TAY) Implied Operand0 2
-- TSX: http://6502.org/users/obelisk/6502/reference.html#TSX
toOpcode 0xba = Right $ Opcode (Official TSX) Implied Operand0 2
-- TXA: http://6502.org/users/obelisk/6502/reference.html#TXA
toOpcode 0x8a = Right $ Opcode (Official TXA) Implied Operand0 2
-- TXS: http://6502.org/users/obelisk/6502/reference.html#TXS
toOpcode 0x9a = Right $ Opcode (Official TXS) Implied Operand0 2
-- TYA: http://6502.org/users/obelisk/6502/reference.html#TYA
toOpcode 0x98 = Right $ Opcode (Official TYA) Implied Operand0 2
toOpcode _ = Left (OpcodeError UnknownOpcode)
