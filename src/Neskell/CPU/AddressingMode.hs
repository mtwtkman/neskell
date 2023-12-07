module Neskell.CPU.AddressingMode where

-- ref: http://6502.org/users/obelisk/6502/addressing.html
data AddressingMode
  = Implied
  | Accumulator
  | Immediate
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | Relative
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | Indirect
  | IndexedIndirect
  | IndirectIndexed
  deriving (Eq, Show)
