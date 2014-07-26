
module LahnParty.GCC.ExecTest where

import Test.HUnitPlus hiding (run)

import LahnParty.GCC
import LahnParty.GCC.AssemblerTest


-- * Test suite

execTests = testSuite "GCC execution tests" [
    test_fall, test_stop,
    test_push, test_add, test_arithmetic,
    test_logic, test_atom,
    test_cons, test_car, test_cdr,
    test_branch,
    test_call,
    test_natsub
  ]


-- * Helper functions

assertError :: Error -> Program -> Assertion
assertError e p = Left e @=? runProgram p

assertStackD :: DataStack -> Program -> Assertion
assertStackD s p = case runProgram p of
  Right gcc -> s @=? _stackD gcc
  Left  err -> assertFailure $ "Got error: " ++ show err

assertStackC :: ControlStack -> Program -> Assertion
assertStackC s p = case runProgram p of
  Right gcc -> s @=? _stackC gcc
  Left  err -> assertFailure $ "Got error: " ++ show err

assertStackCD :: ControlStack -> DataStack -> Program -> Assertion
assertStackCD c d p = case runProgram p of
  Right gcc -> (c,d) @=? (_stackC gcc, _stackD gcc)
  Left  err -> assertFailure $ "Got error: " ++ show err


-- * Tests

test_fall = testName "fall" $ assertError IllegalPC   (program [])
test_stop = testName "stop" $ assertStackCD [Stop] [] (program [RTN])

test_push = testName "push" $ assertStackD [Lit 3, Lit 2]
  $ program [LDC 2, LDC 3, RTN]
test_add  = testName "add"  $ assertStackD [Lit (-3)]
  $ program [LDC 2, LDC (-5), ADD, RTN]

test_arithmetic = testName "arithmetic" $ assertStackD [Lit 10]
  $ program
  [LDC 2, LDC 3, ADD, -- 5
   LDC 4, MUL,        -- 20
   LDC 9, LDC 4, SUB, -- 20 5
   LDC 3, SUB,        -- 20 2
   DIV  , RTN]        -- 10

test_logic = testName "logic" $ assertStackD (map Lit [1,0,1,1,0])
  $ program
  [LDC 5,    LDC 7,    CGTE, -- 0
   LDC 5,    LDC (-7), CGTE, -- 0 1
   LDC 3,    LDC 3,    CGTE, -- 0 1 1
   LDC (-3), LDC (-3), CGT,  -- 0 1 1 0
   LDC 5,    LDC 5,    CEQ,  -- 0 1 1 0 1
   RTN]

test_atom = testName "atom" $ assertStackD [Lit 0, Lit 1]
  $ program [LDC 4, ATOM, LDC 5, LDC 6, CONS, ATOM, RTN]

test_cons = testName "cons" $ assertStackD 
  [Pair (Lit 1) (Pair (Lit 2) (Lit 3))]
  $ program [LDC 1, LDC 2, LDC 3, CONS, CONS, RTN]

test_car = testName "car" $ assertStackD [Lit 1]
  $ program [LDC 1, LDC 2, LDC 3, CONS, CONS, CAR, RTN]

test_cdr = testName "cdr" $ assertStackD [Pair (Lit 2) (Lit 3)]
  $ program [LDC 1, LDC 2, LDC 3, CONS, CONS, CDR, RTN]

test_branch = testName "branch" $ assertStackD [Lit 3, Lit 2]
  $ program
  {- 0: -} [LDC 1, SEL 5 7, LDC 0, SEL 5 7, RTN,
  {- 5: -}  LDC 2, JOIN,
  {- 7: -}  LDC 3, JOIN]

test_call = testName "function call" $ assertStackD
  [Lit 4, Pair (Lit 3) (Pair (Lit 1) (Lit 2))]
  $ program
  {- 0: -} [LDC 1, LDC 2, CONS, LDC 3, -- (1,2) 3
  {- 4: -}  LDF 8, AP 2,               -- call concatenative cons fun
  {- 6: -}  LDC 4, RTN,                -- (3,(1,2)) 4, exit
  {- 8: -}  LD 0 1, LD 0 0, CONS, RTN] -- concatenative cons fun definition

test_natsub = testName "natsub" $ assertStackD [Lit 0, Lit 3] 
  $ assemble natsubCall
