icfp2014
========

University of Marburg - ICFP Programming Contest 2014


##GHC Emulation

Machine.hs contains an emulator for the ghc microcontroller
The function run will run a given state until it halts, while the
function singleInstr will run a single Instruction. This is useful
for using ghci as a debugger.
The machine state can be inspected by using the functions defined in 
the record syntax, so 'reg state' will print the contents of the 
registers of _'state'

TestState.hs shows an example encoding of a program as a 
machine State

Syntax.hs contains the full Syntax of the assembly language.


