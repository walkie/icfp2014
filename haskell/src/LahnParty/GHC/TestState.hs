module Lahnparty.GHC.TestProg where

import Lahnparty.GHC.Simulation
import Lahnparty.GHC.Syntax
import Data.Array.IArray
import Data.Word

{-- base data Memory --}
baseDataM :: Array Word8 Word8
baseDataM = array (0 :: Word8, 255 :: Word8) 
    (take 256 $ zip [0 :: Word8 .. ] (repeat (0::Word8)))

{-- base Program Memory --}
baseProgM :: Array Word8 Instructions
baseProgM = array (0 :: Word8, 255 :: Word8) 
    (take 256 $ zip [0 :: Word8 .. ] 
        (repeat (MOV (MRegister PC) (SRegister PC))))

baseRegister :: GHCRegister 
baseRegister = GHCRegister 0 0 0 0 0 0 0 0

testState :: GHCState
testState = GHCState { 
        datam = baseDataM,
        progm = fillProg,
        reg   = baseRegister,
        pc = 0,
        instr = 0,
        executionOver = False
    }

---
--- some example programs
---
fillProg = baseProgM //
    [
    ( 0, (INC (DRegister C)) ),
    ( 1, (MOV (MRegister (GP E)) (SRegister (GP C))) ) ,
    ( 2, (INC (DIndirectRegister C)) ),
    ( 3, (MOV (MRegister (GP F)) (SIndirectRegister C) ) ) ,
    ( 4, (JEQ (TConstant 0) (SIndirectRegister C) (SConstant 255)) ) ,
    ( 5, (JEQ (TConstant 2) (SConstant 0) (SConstant 0)) ) ]
