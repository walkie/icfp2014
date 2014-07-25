module Lahnparty.GHC.Simulation where

import Lahnparty.GHC.Syntax
import Data.Array.IArray
import Data.Word

type ProgramCounter = Word8
data GHCRegister = GHCRegister {
    rA :: Word8,
    rB :: Word8,
    rC :: Word8,
    rD :: Word8,
    rE :: Word8,
    rF :: Word8,
    rG :: Word8,
    rH :: Word8
    }

type InstructionCounter = Int
--should be fixed in size
type DataMemory    = Array Word8 Word8
type ProgramMemory = Array Word8 Instructions
data GHCState = GHCState {
    datam :: DataMemory, 
    progm :: ProgramMemory, 
    reg :: GHCRegister, 
    pc :: ProgramCounter, 
    instr :: InstructionCounter}


run :: GHCState -> GHCState 
run state = undefined

singleInstr :: GHCState -> GHCState
singleInstr state =
    case ((progm state)!(pc state)) of 
        (MOV dst src)   ->  state
            --case (src) of 
        (INC a)     -> state
        (DEC a)     -> state
        (ADD a b)   -> state
        (SUB a b)   -> state
        (MUL a b)   -> state
        (AND a b)   -> state
        (OR  a b)   -> state
        (XOR a b)   -> state
        (JLT a b c) -> state
        (JEQ a b c) -> state
        (JGT a b c) -> state
        (INT a)     -> state
        (HLT)       -> state 

getSourceVal :: Argument -> GHCState ->  Word8
getSourceVal src state = 
    case src of
       (Constant x)      -> x
       (Register (GP A))        -> rA $ reg state
       (Register (GP B))        -> rB $ reg state
       (Register (GP C))        -> rC $ reg state
       (Register (GP D))        -> rD $ reg state
       (Register (GP E))        -> rE $ reg state
       (Register (GP F))        -> rF $ reg state
       (Register (GP G))        -> rG $ reg state
       (Register (GP H))        -> rH $ reg state
       (Register PC)            -> pc state
       (DataMemory x)    -> (datam state)!x
       (IndirectRegister r)-> (datam state)!(getSourceVal (Register (GP r)) state)

setDest :: Argument -> Word8 -> GHCState -> GHCState
setDest dest val = 
       
