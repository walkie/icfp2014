
module Main where

import Test.HUnitPlus.Main

import LahnParty.GCC.ExecTest
import LahnParty.GCC.AssemblerTest

main = createMain [assemblerTests,execTests]
