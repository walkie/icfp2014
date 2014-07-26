
module LahnParty.GCC.ExecTest where

import Test.HUnitPlus hiding (run)

import LahnParty.GCC


-- * Test suite

execTests = testSuite "GCC execution tests" [
    test_initStackD, test_initStackC,
    test_push, test_add,
    test_arithmetic
  ]


-- * Helper functions

run :: [Inst] -> (Maybe Error, GCC)
run = runProgram . program

assertError :: Error -> [Inst] -> Assertion
assertError e p = Just e @=? fst (run p)

assertStackD :: DataStack -> [Inst] -> Assertion
assertStackD s p = s @=? _stackD (snd (run p))

assertStackC :: ControlStack -> [Inst] -> Assertion
assertStackC s p = s @=? _stackC (snd (run p))

assertStackCD :: ControlStack -> DataStack -> [Inst] -> Assertion
assertStackCD cs ds p = (cs,ds) @=? (_stackC gcc, _stackD gcc)
  where gcc = snd (run p)


-- * Tests

test_initStackD = testName "initStackD" $ assertStackD [] []
test_initStackC = testName "initStackC" $ assertStackC [Stop] []

test_push = testName "push" $ assertStackD [Lit 3, Lit 2] [LDC 2, LDC 3]
test_add  = testName "add"  $ assertStackD [Lit (-3)] [LDC 2, LDC (-5), ADD]

test_arithmetic = testName "arithmetic" $ assertStackD [Lit 10]
  [LDC 2, LDC 3, ADD, -- 5
   LDC 4, MUL,        -- 20
   LDC 9, LDC 4, SUB, -- 20, 5
   LDC 3, SUB,        -- 20, 2
   DIV]               -- 10
