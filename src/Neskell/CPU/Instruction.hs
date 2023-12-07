module Neskell.CPU.Instruction where

-- ref: http://6502.org/users/obelisk/6502/reference.html#NOP
data Instruction = Official Official | Unofficial Unofficial deriving (Eq, Show)

data Official
  = ADC
  | AND
  | ASL
  | BCC
  | BCS
  | BEQ
  | BIT
  | BMI
  | BNE
  | BPL
  | BRK
  | BVC
  | BVS
  | CLC
  | CLD
  | CLI
  | CLV
  | CMP
  | CPX
  | CPY
  | DEC
  | DEX
  | DEY
  | EOR
  | INC
  | INX
  | INY
  | JMP
  | JSR
  | LDA
  | LDX
  | LDY
  | LSR
  | NOP
  | ORA
  | PHA
  | PHP
  | PLA
  | PLP
  | ROL
  | ROR
  | RTI
  | RTS
  | SBC
  | SEC
  | SED
  | SEI
  | STA
  | STX
  | STY
  | TAX
  | TAY
  | TSX
  | TXA
  | TXS
  | TYA
  deriving (Eq, Show)

data Unofficial
  = ALR
  | ANC
  | ARR
  | AXS
  | DCP
  | IGN
  | ISC
  | LAX
  | UN_NOP
  | RLA
  | RRA
  | SAX
  | SKB
  | SLO
  | SRE
  | STP
  deriving (Eq, Show)
