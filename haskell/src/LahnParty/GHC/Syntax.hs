module Lahnparty.GHC.Syntax where

import Data.Word

data MovArgument = 
      MRegister RegisterName
    | MIndirectRegister GPRegisterName
    | MDataMemory Word8 deriving (Show)
    
data DestArgument = 
      DRegister GPRegisterName 
    | DIndirectRegister GPRegisterName
    | DDataMemory Word8 deriving (Show)

data SrcArgument = 
      SRegister RegisterName
    | SIndirectRegister GPRegisterName
    | SConstant   Word8 
    | SDataMemory Word8 deriving (Show)

data TArgument = TConstant Word8 deriving (Show)

data GPRegisterName = A | B | C | D | E | F | G | H deriving (Show)
data RegisterName =  GP GPRegisterName | PC deriving (Show)

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
    | HLT deriving (Show)

data Interrupt = Int0 | Int1 | Int2 | Int3 | Int4 | Int5 | Int6 | Int7 
    deriving (Show)
