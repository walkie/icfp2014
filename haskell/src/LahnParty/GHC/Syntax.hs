module Lahnparty.GHC.Syntax where

import Data.Word

data MovArgument = 
      MRegister RegisterName
    | MIndirectRegister GPRegisterName
    | MDataMemory Word8
    
data DestArgument = 
      DRegister GPRegisterName 
    | DIndirectRegister GPRegisterName
    | DDataMemory Word8

data SrcArgument = 
      SRegister RegisterName
    | SIndirectRegister GPRegisterName
    | SConstant   Word8 
    | SDataMemory Word8 

data TArgument = TConstant Word8

data GPRegisterName = A | B | C | D | E | F | G | H 
data RegisterName =  GP GPRegisterName | PC

data Instructions = 
      MOV MovArgument SrcArgument 
    | INC DestArgument
    | DEC DestArgument
    | ADD DestArgument SrcArgument
    | SUB DestArgument SrcArgument
    | MUL DestArgument SrcArgument
    | DIV DestArgument SrcArgument
    | AND DestArgument SrcArgument
    | OR  DestArgument SrcArgument
    | XOR DestArgument SrcArgument
    | JLT TArgument SrcArgument SrcArgument
    | JEQ TArgument SrcArgument SrcArgument
    | JGT TArgument SrcArgument SrcArgument
    | INT Interrupt
    | HLT

data Interrupt = Int0 | Int1 | Int2 | Int3 | Int4 | Int5 | Int6 | Int7 
