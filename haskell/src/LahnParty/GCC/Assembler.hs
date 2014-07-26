
module LahnParty.GCC.Assembler where

import LahnParty.GCC.Syntax


-- | A label to a block in an assembly program.
type Label = String

-- | An assembly program is a list of labeled instruction blocks.
type Assembly = [(Label,[Inst Label])]

-- | Get the address of a label.
address :: Assembly -> Label -> Addr
address ((m,b):t) l
  | l == m    = 0
  | otherwise = length b + address t l
address [] l = error $ "undefined label: " ++ l

-- | Translate an assembly program into a program with fixed addresses.
assemble :: Assembly -> Program
assemble p = program (map inst (concatMap snd p))
  where
    -- instructions that require resolving labels
    inst (SEL  t f) = SEL  (address p t) (address p f)
    inst (TSEL t f) = TSEL (address p t) (address p f)
    inst (LDF  f)   = LDF  (address p f)
    -- the rest of the instructions
    inst (LD n i) = (LD n i)
    inst ADD      = ADD     
    inst SUB      = SUB     
    inst MUL      = MUL     
    inst DIV      = DIV     
    inst CEQ      = CEQ     
    inst CGT      = CGT     
    inst CGTE     = CGTE    
    inst ATOM     = ATOM    
    inst CONS     = CONS    
    inst CAR      = CAR     
    inst CDR      = CDR     
    inst JOIN     = JOIN    
    inst (AP  n)  = (AP  n) 
    inst RTN      = RTN     
    inst (DUM n)  = (DUM n) 
    inst (RAP n)  = (RAP n) 
    inst STOP     = STOP    
    inst (TAP  n) = (TAP  n)
    inst (TRAP n) = (TRAP n)
    inst (ST n i) = (ST n i)
    inst DBUG     = DBUG    
    inst BRK      = BRK     
