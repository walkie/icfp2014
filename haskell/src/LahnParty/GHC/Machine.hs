{-- 
 - Simulates the Operation of the GHC microcontroller
 --}
module LahnParty.GHC.Machine where

import LahnParty.GHC.Syntax
import Data.Array.IArray
import Data.Word
import Data.Bits

maxSteps = 1023

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
    } deriving (Show)

type InstructionCounter = Int
--should be fixed in size
type DataMemory    = Array Word8 Word8
type ProgramMemory = Array Word8 Instructions

data GHCState = GHCState {
    datam :: DataMemory, 
    progm :: ProgramMemory, 
    reg :: GHCRegister, 
    pc :: ProgramCounter, 
    instr :: InstructionCounter,
    executionOver :: Bool} 

run :: GHCState -> GHCState 
run state = 
    if (not (executionOver state)) then 
        ((run.singleInstr) state) else state

singleInstr :: GHCState -> GHCState
singleInstr state =
    let executeBinOP src dst op =
            setDest dst (op (getSourceVal src state) (getDestVal dst state)) state 
        conditionaljump t x y condition = 
                if (condition (getSourceVal x state) (getSourceVal y state)) 
                    then (setMov (MRegister PC) t state) else state
        step = if (executionOver state) then state else  
            case ((progm state)!(pc state)) of 
                (MOV dst src)   -> setMov  dst (getSourceVal src state) state 
                (INC dst)       -> setDest dst ((getDestVal dst state)+1) state
                (DEC dst)       -> setDest dst ((getDestVal dst state)-1) state
                (ADD dst src)   -> executeBinOP src dst (+)
                (SUB dst src)   -> executeBinOP src dst (-)
                (MUL dst src)   -> executeBinOP src dst (*)
                (DIV dst src)   -> executeBinOP src dst div
                (AND dst src)   -> executeBinOP src dst (.&.)
                (OR  dst src)   -> executeBinOP src dst (.|.)
                (XOR  dst src)  -> executeBinOP src dst xor
                (JLT (TConstant t) x y)  -> conditionaljump t x y (<)
                (JEQ (TConstant t) x y)  -> conditionaljump t x y (==) 
                (JGT (TConstant t) x y)  -> conditionaljump t x y (>)
                (INT a)      -> state
                (HLT)        -> state {executionOver = True} 
        afterstate = if ((pc step)==(pc state)&&(not $ executionOver step)) 
                        then step {pc = (pc step)+1}
                        else step
        increaseIc = 
            if (executionOver afterstate) then afterstate else 
                afterstate {instr = (instr afterstate)+1}
        ifOver st = if ( (instr st) > maxSteps ) then 
            st {executionOver = True} else st
        
    in 
        ifOver  increaseIc

getSourceVal :: SrcArgument -> GHCState ->  Word8
getSourceVal src state = 
    case src of
       (SConstant x)      -> x
       (SRegister (GP A))        -> rA $ reg state
       (SRegister (GP B))        -> rB $ reg state
       (SRegister (GP C))        -> rC $ reg state
       (SRegister (GP D))        -> rD $ reg state
       (SRegister (GP E))        -> rE $ reg state
       (SRegister (GP F))        -> rF $ reg state
       (SRegister (GP G))        -> rG $ reg state
       (SRegister (GP H))        -> rH $ reg state
       (SRegister PC)            -> pc state
       (SDataMemory x)    -> (datam state)!x
       (SIndirectRegister r)-> 
            (datam state)!(getSourceVal (SRegister (GP r)) state)

getDestVal :: DestArgument -> GHCState -> Word8
getDestVal dst state = 
    case dst of
       (DRegister (A))        -> rA $ reg state
       (DRegister (B))        -> rB $ reg state
       (DRegister (C))        -> rC $ reg state
       (DRegister (D))        -> rD $ reg state
       (DRegister (E))        -> rE $ reg state
       (DRegister (F))        -> rF $ reg state
       (DRegister (G))        -> rG $ reg state
       (DRegister (H))        -> rH $ reg state
       (DDataMemory x)    -> (datam state)!x
       (DIndirectRegister r)-> 
            (datam state)!(getDestVal (DRegister (r)) state)

setDest :: DestArgument -> Word8 -> GHCState -> GHCState
setDest dest val state = 
    case dest of
       (DRegister (A))        -> state {reg = (reg state) {rA = val}}
       (DRegister (B))        -> state {reg = (reg state) {rB = val}}
       (DRegister (C))        -> state {reg = (reg state) {rC = val}}
       (DRegister (D))        -> state {reg = (reg state) {rD = val}}
       (DRegister (E))        -> state {reg = (reg state) {rE = val}}
       (DRegister (F))        -> state {reg = (reg state) {rF = val}}
       (DRegister (G))        -> state {reg = (reg state) {rG = val}}
       (DRegister (H))        -> state {reg = (reg state) {rH = val}}   
       (DIndirectRegister r)  -> 
            state {datam = 
            (datam state) // [((getDestVal (DRegister r) state), val)] }
       (DDataMemory x)        -> state {datam = (datam state)//[(x,val)]}

setMov :: MovArgument -> Word8 -> GHCState -> GHCState 
setMov dest val state = 
    case dest of 
        (MRegister PC)        -> state {pc = val}
        (MRegister (GP x))    -> setDest (DRegister x) val state
        (MIndirectRegister x) -> setDest (DIndirectRegister x) val state
        (MDataMemory x)       -> setDest (DDataMemory x) val state
