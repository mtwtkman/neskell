module Neskell.CPU.Opcode where

import Data.Word (Word16)
import Neskell.CPU.AddressingMode (AddressingMode (..))
import Neskell.CPU.Instruction (Instruction (..), Official (..), Unofficial (..))

data Opcode = Opcode
  { opInstruction :: Instruction,
    opAddressingMode :: Maybe AddressingMode,
    opBytesSize :: Int,
    opBaseCycle :: Int
  }

uncertain :: Int
uncertain = 0

toOpcode :: Word16 -> Opcode
-- ADC: https://6502.org/users/obelisk/6502/reference.html#ADC
toOpcode 0x69 = Opcode (Official ADC) (Just Immediate) 2 2
toOpcode 0x65 = Opcode (Official ADC) (Just ZeroPage) 2 3
toOpcode 0x75 = Opcode (Official ADC) (Just ZeroPageX) 2 4
toOpcode 0x6d = Opcode (Official ADC) (Just Absolute) 3 4
toOpcode 0x7d = Opcode (Official ADC) (Just AbsoluteX) 3 4
toOpcode 0x79 = Opcode (Official ADC) (Just AbsoluteY) 3 4
toOpcode 0x61 = Opcode (Official ADC) (Just IndexedIndirect) 2 6
toOpcode 0x71 = Opcode (Official ADC) (Just IndirectIndexed) 2 5
-- AND: https://6502.org/users/obelisk/6502/reference.html#AND
toOpcode 0x29 = Opcode (Official AND) (Just Immediate) 2 2
toOpcode 0x25 = Opcode (Official AND) (Just ZeroPage) 2 3
toOpcode 0x35 = Opcode (Official AND) (Just ZeroPageX) 2 4
toOpcode 0x2d = Opcode (Official AND) (Just Absolute) 3 4
toOpcode 0x3d = Opcode (Official AND) (Just AbsoluteX) 3 4
toOpcode 0x39 = Opcode (Official AND) (Just AbsoluteY) 3 4
toOpcode 0x21 = Opcode (Official AND) (Just IndexedIndirect) 2 6
toOpcode 0x31 = Opcode (Official AND) (Just IndirectIndexed) 2 5
-- ASL: https://6502.org/users/obelisk/6502/reference.html#ASL
toOpcode 0x0a = Opcode (Official ASL) (Just Accumulator) 1 2
toOpcode 0x06 = Opcode (Official ASL) (Just ZeroPage) 2 5
toOpcode 0x16 = Opcode (Official ASL) (Just ZeroPageX) 2 6
toOpcode 0x0e = Opcode (Official ASL) (Just Absolute) 3 6
toOpcode 0x1e = Opcode (Official ASL) (Just AbsoluteX) 3 7
-- BCC: https://6502.org/users/obelisk/6502/reference.html#BCC
toOpcode 0x90 = Opcode (Official BCC) (Just Relative) 2 2
-- BCS: https://6502.org/users/obelisk/6502/reference.html#BCS
toOpcode 0xb0 = Opcode (Official BCS) (Just Relative) 2 2
-- BEQ: https://6502.org/users/obelisk/6502/reference.html#BEQ
toOpcode 0xf0 = Opcode (Official BEQ) (Just Relative) 2 2
-- BIT: https://6502.org/users/obelisk/6502/reference.html#BIT
toOpcode 0x24 = Opcode (Official BIT) (Just ZeroPage) 2 3
toOpcode 0x2c = Opcode (Official BIT) (Just Absolute) 3 4
-- BMI: https://6502.org/users/obelisk/6502/reference.html#BMI
toOpcode 0x30 = Opcode (Official BMI) (Just Relative) 2 2
-- BNE: https://6502.org/users/obelisk/6502/reference.html#BNE
toOpcode 0xd0 = Opcode (Official BNE) (Just Relative) 2 2
-- BPL: https://6502.org/users/obelisk/6502/reference.html#BPL
toOpcode 0x10 = Opcode (Official BPL) (Just Relative) 2 2
-- BRK: https://6502.org/users/obelisk/6502/reference.html#BRK
toOpcode 0x00 = Opcode (Official BRK) (Just Implied) 1 7
-- BVC: https://6502.org/users/obelisk/6502/reference.html#BVC
toOpcode 0x50 = Opcode (Official BVC) (Just Relative) 2 2
-- BVS: https://6502.org/users/obelisk/6502/reference.html#BVS
toOpcode 0x70 = Opcode (Official BVS) (Just Relative) 2 2
-- CLC: https://6502.org/users/obelisk/6502/reference.html#CLC
toOpcode 0x18 = Opcode (Official CLC) (Just Implied) 1 2
-- CLD: https://6502.org/users/obelisk/6502/reference.html#CLD
toOpcode 0xd8 = Opcode (Official CLD) (Just Implied) 1 2
-- CLI: https://6502.org/users/obelisk/6502/reference.html#CLI
toOpcode 0x58 = Opcode (Official CLI) (Just Implied) 1 2
-- CLV: https://6502.org/users/obelisk/6502/reference.html#CLV
toOpcode 0xb8 = Opcode (Official CLV) (Just Implied) 1 2
-- CMP: https://6502.org/users/obelisk/6502/reference.html#CMP
toOpcode 0xc9 = Opcode (Official CMP) (Just Immediate) 2 2
toOpcode 0xc5 = Opcode (Official CMP) (Just ZeroPage) 2 3
toOpcode 0xd5 = Opcode (Official CMP) (Just ZeroPageX) 2 4
toOpcode 0xcd = Opcode (Official CMP) (Just Absolute) 3 4
toOpcode 0xdd = Opcode (Official CMP) (Just AbsoluteX) 3 4
toOpcode 0xd9 = Opcode (Official CMP) (Just AbsoluteY) 3 4
toOpcode 0xc1 = Opcode (Official CMP) (Just IndexedIndirect) 2 6
toOpcode 0xd1 = Opcode (Official CMP) (Just IndirectIndexed) 2 5
-- CPX: https://6502.org/users/obelisk/6502/reference.html#CPX
toOpcode 0xe0 = Opcode (Official CPX) (Just Immediate) 2 2
toOpcode 0xe4 = Opcode (Official CPX) (Just ZeroPage) 2 3
toOpcode 0xec = Opcode (Official CPX) (Just Absolute) 3 4
-- CPY: https://6502.org/users/obelisk/6502/reference.html#CPY
toOpcode 0xc0 = Opcode (Official CPY) (Just Immediate) 2 2
toOpcode 0xc4 = Opcode (Official CPY) (Just ZeroPage) 2 3
toOpcode 0xcc = Opcode (Official CPY) (Just Absolute) 3 4
-- DEC: https://6502.org/users/obelisk/6502/reference.html#DEC
toOpcode 0xc6 = Opcode (Official DEC) (Just ZeroPage) 2 5
toOpcode 0xd6 = Opcode (Official DEC) (Just ZeroPageX) 2 6
toOpcode 0xce = Opcode (Official DEC) (Just Absolute) 3 6
toOpcode 0xde = Opcode (Official DEC) (Just AbsoluteX) 3 7
-- DEX: https://6502.org/users/obelisk/6502/reference.html#DEX
toOpcode 0xca = Opcode (Official DEC) (Just Implied) 1 2
-- DEY: https://6502.org/users/obelisk/6502/reference.html#DEY
toOpcode 0x88 = Opcode (Official DEC) (Just Implied) 1 2
-- EOR: https://6502.org/users/obelisk/6502/reference.html#EOR
toOpcode 0x49 = Opcode (Official EOR) (Just Immediate) 2 2
toOpcode 0x45 = Opcode (Official EOR) (Just ZeroPage) 2 3
toOpcode 0x55 = Opcode (Official EOR) (Just ZeroPageX) 2 4
toOpcode 0x4d = Opcode (Official EOR) (Just Absolute) 3 4
toOpcode 0x5d = Opcode (Official EOR) (Just AbsoluteX) 3 4
toOpcode 0x59 = Opcode (Official EOR) (Just AbsoluteY) 3 4
toOpcode 0x41 = Opcode (Official EOR) (Just IndexedIndirect) 2 6
toOpcode 0x51 = Opcode (Official EOR) (Just IndirectIndexed) 2 5
-- INC: https://6502.org/users/obelisk/6502/reference.html#INC
toOpcode 0xe6 = Opcode (Official INC) (Just ZeroPage) 2 5
toOpcode 0xf6 = Opcode (Official INC) (Just ZeroPageX) 2 6
toOpcode 0xee = Opcode (Official INC) (Just Absolute) 3 6
toOpcode 0xfe = Opcode (Official INC) (Just AbsoluteX) 3 7
-- INX: https://6502.org/users/obelisk/6502/reference.html#INX
toOpcode 0xe8 = Opcode (Official INX) (Just Implied) 1 2
-- INY: https://6502.org/users/obelisk/6502/reference.html#INY
toOpcode 0xc8 = Opcode (Official INY) (Just Implied) 1 2
-- JMP: https://6502.org/users/obelisk/6502/reference.html#JMP
toOpcode 0x4c = Opcode (Official JMP) (Just Absolute) 3 3
toOpcode 0x6c = Opcode (Official JMP) (Just Indirect) 3 5
-- JSR: https://6502.org/users/obelisk/6502/reference.html#JSR
toOpcode 0x20 = Opcode (Official JSR) (Just Absolute) 3 6
-- LDA: https://6502.org/users/obelisk/6502/reference.html#LDA
toOpcode 0xa9 = Opcode (Official LDA) (Just Immediate) 2 2
toOpcode 0xa5 = Opcode (Official LDA) (Just ZeroPage) 2 3
toOpcode 0xb5 = Opcode (Official LDA) (Just ZeroPageX) 2 4
toOpcode 0xad = Opcode (Official LDA) (Just Absolute) 3 4
toOpcode 0xbd = Opcode (Official LDA) (Just AbsoluteX) 3 4
toOpcode 0xb9 = Opcode (Official LDA) (Just AbsoluteY) 3 4
toOpcode 0xa1 = Opcode (Official LDA) (Just IndexedIndirect) 2 6
toOpcode 0xb1 = Opcode (Official LDA) (Just IndirectIndexed) 2 5
-- LDX: https://6502.org/users/obelisk/6502/reference.html#LDX
toOpcode 0xa2 = Opcode (Official LDX) (Just Immediate) 2 2
toOpcode 0xa6 = Opcode (Official LDX) (Just ZeroPage) 2 3
toOpcode 0xb6 = Opcode (Official LDX) (Just ZeroPageY) 2 4
toOpcode 0xae = Opcode (Official LDX) (Just Absolute) 3 4
toOpcode 0xbe = Opcode (Official LDX) (Just AbsoluteY) 3 4
-- LDY: https://6502.org/users/obelisk/6502/reference.html#LDY
toOpcode 0xa0 = Opcode (Official LDY) (Just Immediate) 2 2
toOpcode 0xa4 = Opcode (Official LDY) (Just ZeroPage) 2 3
toOpcode 0xb4 = Opcode (Official LDY) (Just ZeroPageX) 2 4
toOpcode 0xac = Opcode (Official LDY) (Just Absolute) 3 4
toOpcode 0xbc = Opcode (Official LDY) (Just AbsoluteX) 3 4
-- LSR: https://6502.org/users/obelisk/6502/reference.html#LSR
toOpcode 0x4a = Opcode (Official LSR) (Just Accumulator) 1 2
toOpcode 0x46 = Opcode (Official LSR) (Just ZeroPage) 2 5
toOpcode 0x56 = Opcode (Official LSR) (Just ZeroPageX) 2 6
toOpcode 0x4e = Opcode (Official LSR) (Just Absolute) 3 6
toOpcode 0x5e = Opcode (Official LSR) (Just AbsoluteX) 3 7
-- NOP: https://6502.org/users/obelisk/6502/reference.html#NOP
toOpcode 0xea = Opcode (Official NOP) (Just Implied) 1 2
-- ORA: https://6502.org/users/obelisk/6502/reference.html#ORA
toOpcode 0x09 = Opcode (Official ORA) (Just Immediate) 2 2
toOpcode 0x05 = Opcode (Official ORA) (Just ZeroPage) 2 3
toOpcode 0x15 = Opcode (Official ORA) (Just ZeroPageX) 2 4
toOpcode 0x0d = Opcode (Official ORA) (Just Absolute) 3 4
toOpcode 0x1d = Opcode (Official ORA) (Just AbsoluteX) 3 4
toOpcode 0x19 = Opcode (Official ORA) (Just AbsoluteY) 3 4
toOpcode 0x01 = Opcode (Official ORA) (Just IndexedIndirect) 2 6
toOpcode 0x11 = Opcode (Official ORA) (Just IndirectIndexed) 2 5
-- PHA: https://6502.org/users/obelisk/6502/reference.html#PHA
toOpcode 0x48 = Opcode (Official PHA) (Just Implied) 1 3
-- PHP: https://6502.org/users/obelisk/6502/reference.html#PHP
toOpcode 0x68 = Opcode (Official PHP) (Just Implied) 1 4
-- PLP: https://6502.org/users/obelisk/6502/reference.html#PLP
toOpcode 0x28 = Opcode (Official PLP) (Just Implied) 1 4
-- ROL: https://6502.org/users/obelisk/6502/reference.html#ROL
toOpcode 0x2a = Opcode (Official ROL) (Just Accumulator) 1 2
toOpcode 0x26 = Opcode (Official ROL) (Just ZeroPage) 2 5
toOpcode 0x36 = Opcode (Official ROL) (Just ZeroPageX) 2 6
toOpcode 0x2e = Opcode (Official ROL) (Just Absolute) 3 6
toOpcode 0x3e = Opcode (Official ROL) (Just AbsoluteX) 3 7
-- RO$: https://6502.org/users/obelisk/6502/reference.html#ROR
toOpcode 0x6a = Opcode (Official ROR) (Just Accumulator) 1 2
toOpcode 0x66 = Opcode (Official ROR) (Just ZeroPage) 2 5
toOpcode 0x76 = Opcode (Official ROR) (Just ZeroPageX) 2 6
toOpcode 0x6e = Opcode (Official ROR) (Just Absolute) 3 6
toOpcode 0x7e = Opcode (Official ROR) (Just AbsoluteX) 3 7
-- RTI: https://6502.org/users/obelisk/6502/reference.html#RTI
toOpcode 0x40 = Opcode (Official RTI) (Just Implied) 1 6
-- RTS: https://6502.org/users/obelisk/6502/reference.html#RTS
toOpcode 0x60 = Opcode (Official RTS) (Just Implied) 1 6
-- SBC: https://6502.org/users/obelisk/6502/reference.html#SBC
toOpcode 0xe9 = Opcode (Official SBC) (Just Immediate) 2 2
toOpcode 0xe5 = Opcode (Official SBC) (Just ZeroPage) 2 3
toOpcode 0xf5 = Opcode (Official SBC) (Just ZeroPageX) 2 4
toOpcode 0xed = Opcode (Official SBC) (Just Absolute) 3 4
toOpcode 0xfd = Opcode (Official SBC) (Just AbsoluteX) 3 4
toOpcode 0xf9 = Opcode (Official SBC) (Just AbsoluteY) 3 4
toOpcode 0xe1 = Opcode (Official SBC) (Just IndexedIndirect) 2 6
toOpcode 0xf1 = Opcode (Official SBC) (Just IndirectIndexed) 2 5
-- SEC: https://6502.org/users/obelisk/6502/reference.html#SEC
toOpcode 0x38 = Opcode (Official SEC) (Just Implied) 1 2
-- SED: https://6502.org/users/obelisk/6502/reference.html#SED
toOpcode 0xf8 = Opcode (Official SED) (Just Implied) 1 2
-- SEI: https://6502.org/users/obelisk/6502/reference.html#SEI
toOpcode 0x78 = Opcode (Official SEI) (Just Implied) 1 2
-- STA: https://6502.org/users/obelisk/6502/reference.html#STA
toOpcode 0x85 = Opcode (Official STA) (Just ZeroPage) 2 3
toOpcode 0x95 = Opcode (Official STA) (Just ZeroPageX) 2 4
toOpcode 0x8d = Opcode (Official STA) (Just Absolute) 3 4
toOpcode 0x9d = Opcode (Official STA) (Just AbsoluteX) 3 5
toOpcode 0x99 = Opcode (Official STA) (Just AbsoluteY) 3 5
toOpcode 0x81 = Opcode (Official STA) (Just IndexedIndirect) 2 6
toOpcode 0x91 = Opcode (Official STA) (Just IndirectIndexed) 2 6
-- STX: https://6502.org/users/obelisk/6502/reference.html#STX
toOpcode 0x86 = Opcode (Official STX) (Just ZeroPage) 2 3
toOpcode 0x96 = Opcode (Official STX) (Just ZeroPageY) 2 4
toOpcode 0x8e = Opcode (Official STX) (Just Absolute) 3 4
-- STY: https://6502.org/users/obelisk/6502/reference.html#STY
toOpcode 0x84 = Opcode (Official STY) (Just ZeroPage) 2 3
toOpcode 0x94 = Opcode (Official STY) (Just ZeroPageX) 2 4
toOpcode 0x8c = Opcode (Official STY) (Just Absolute) 3 4
-- TAX: https://6502.org/users/obelisk/6502/reference.html#TAX
toOpcode 0xaa = Opcode (Official TAX) (Just Implied) 1 2
-- TAY: https://6502.org/users/obelisk/6502/reference.html#TAY
toOpcode 0xa8 = Opcode (Official TAY) (Just Implied) 1 2
-- TSX: https://6502.org/users/obelisk/6502/reference.html#TSX
toOpcode 0xba = Opcode (Official TSX) (Just Implied) 1 2
-- TXA: https://6502.org/users/obelisk/6502/reference.html#TXA
toOpcode 0x8a = Opcode (Official TXA) (Just Implied) 1 2
-- TXS: https://6502.org/users/obelisk/6502/reference.html#TXS
toOpcode 0x9a = Opcode (Official TXS) (Just Implied) 1 2
-- TYA: https://6502.org/users/obelisk/6502/reference.html#TYA
toOpcode 0x98 = Opcode (Official TYA) (Just Implied) 1 2
-- Unofficial opcodes
-- ref: https://www.nesdev.org/wiki/Programming_with_unofficial_opcodes
toOpcode 0x4b = Opcode (Unofficial ALR) (Just Immediate) 1 2
toOpcode b
  | b `elem` [0x02, 0x22, 0x42, 0x62, 0x12, 0x32, 0x52, 0x72, 0x92, 0xb2, 0xd2, 0xf2] = Opcode (Unofficial STP) Nothing uncertain uncertain
  | b `elem` [0x80, 0x89] = Opcode (Unofficial UN_NOP) (Just Immediate) uncertain uncertain
  | otherwise = Opcode Unknown Nothing uncertain uncertain
