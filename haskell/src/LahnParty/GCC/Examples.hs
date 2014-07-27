
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


-- | Local variable example from spec.
local :: Assembly
local = 
  [("entry", [ LDC  21
             , LDF  "body"   -- load body
             , AP   1        -- call body with 1 variable in a new frame
             , RTN
             ]),
   ("body",  [ LD   0 0      -- var x
             , LD   0 0      -- var x
             , ADD
             , RTN
             ])]

-- | Mutual recursion example from spec.
goto :: Assembly
goto =
  [("entry", [ DUM  2        -- 2 top-level declarations
             , LDF  "go"     -- declare function go
             , LDF  "to"     -- declare function to
             , LDF  "main"   -- main function
             , RAP  2        -- load declarations into environment and run main
             , RTN           -- final return
             ]),
   ("main",  [ LDC  1
             , LD   0 0      -- var go
             , AP   1        -- call go(1)
             , RTN
             ]),
   ("to",    [ LD   0 0      -- var n
             , LDC  1
             , SUB
             , LD   1 0      -- var go
             , AP   1        -- call go(n-1)
             , RTN
             ]),
   ("go",    [ LD   0 0      -- var n
             , LDC  1
             , ADD
             , LD   1 1      -- var to
             , AP   1        -- call to(n+1)
             , RTN
             ])]
