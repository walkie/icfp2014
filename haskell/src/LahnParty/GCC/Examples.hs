
module LahnParty.GCC.Examples where

import LahnParty.GCC.Syntax
import LahnParty.GCC.Assembler


-- | Assembly definition of natsub: natural number subtraction.
natsubDef :: Assembly
natsubDef =
  [("natsub",  [LD 0 0, LD 0 1, SUB, LDC 0, CGTE, SEL "natsubT" "natsubF", RTN]),
   ("natsubT", [LD 0 0, LD 0 1, SUB, JOIN]),
   ("natsubF", [LDC 0, JOIN])]

-- | Assembly program calling natsub.
natsubCall :: Assembly
natsubCall =
  ("entry", [LDC 7, LDC 4, LDF "natsub", AP 2,
             LDC 4, LDC 7, LDF "natsub", AP 2, RTN]) : natsubDef
