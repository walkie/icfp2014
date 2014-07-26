
module LahnParty.GCC.AssemblerTest where

import Test.HUnitPlus hiding (run)

import LahnParty.GCC


assemblerTests = testSuite "GCC assembler tests" [test_assemble]

test_assemble = testName "assemble" $
  assemble natsubCall @?= program
  {-  0 -} [LDC 7, LDC 4, LDF 9, AP 2,
  {-  4 -}  LDC 4, LDC 7, LDF 9, AP 2, RTN,
  {-  9 -}  LD 0 0, LD 0 1, SUB, LDC 0, CGTE, SEL 16 20, RTN,
  {- 16 -}  LD 0 0, LD 0 1, SUB, JOIN,
  {- 20 -}  LDC 0, JOIN]
