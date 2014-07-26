
module LahnParty.GCC.ExecTest where

import Test.HUnitPlus hiding (run)

import LahnParty.GCC


-- * Test suite

execTests = testSuite "GCC execution tests" [
    test_empty,
    test_push, test_add,
    test_arithmetic
  ]


-- * Helper functions

run :: [Inst] -> Either Error GCC
run = runProgram . program

assertError :: Error -> [Inst] -> Assertion
assertError e p = Left e @=? run p

assertStackD :: DataStack -> [Inst] -> Assertion
assertStackD s p = case run p of
  Right gcc -> s @=? _stackD gcc
  Left  err -> assertFailure $ "Got error: " ++ show err

assertStackC :: ControlStack -> [Inst] -> Assertion
assertStackC s p = case run p of
  Right gcc -> s @=? _stackC gcc
  Left  err -> assertFailure $ "Got error: " ++ show err

assertStackCD :: ControlStack -> DataStack -> [Inst] -> Assertion
assertStackCD c d p = case run p of
  Right gcc -> (c,d) @=? (_stackC gcc, _stackD gcc)
  Left  err -> assertFailure $ "Got error: " ++ show err


-- * Tests

test_fall = testName "fall" $ assertError IllegalPC [] []
test_stop = testName "stop" $ assertStackCD [Stop] [] [RTN]

test_push = testName "push" $ assertStackD [Lit 3, Lit 2] [LDC 2, LDC 3, RTN]
test_add  = testName "add"  $ assertStackD [Lit (-3)] [LDC 2, LDC (-5), ADD, RTN]

test_arithmetic = testName "arithmetic" $ assertStackD [Lit 10]
  [LDC 2, LDC 3, ADD, -- 5
   LDC 4, MUL,        -- 20
   LDC 9, LDC 4, SUB, -- 20, 5
   LDC 3, SUB,        -- 20, 2
   DIV  , RTN]        -- 10
