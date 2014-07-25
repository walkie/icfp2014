module Lahnparty.GHC.Syntax where

import Data.Word

data Instructions = 
      MOV Word8 Word8 
    | INC Word8
    | DEC Word8
    | ADD Word8 Word8
    | SUB Word8 Word8
    | MUL Word8 Word8
    | AND Word8 Word8
    | OR  Word8 Word8
    | XOR Word8 Word8
    | JLT Word8 Word8 Word8
    | JEQ Word8 Word8 Word8
    | JGT Word8 Word8 Word8
    | INT Interrupt
    | HLT

data Interrupt = Int1 | Int2 | Int3 | Int4 | Int5 | Int6 | Int7 
