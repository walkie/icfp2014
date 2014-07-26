
module LahnParty.GCC.Pretty where

import Data.Array
import Data.List (intercalate)

import LahnParty.GCC.Assembler
import LahnParty.GCC.State
import LahnParty.GCC.Syntax


prettyAssembly :: Assembly -> String
prettyAssembly p = unlines (map block p)
  where
    block (l,is) = unlines ((l ++ ":") : map (("  " ++) . inst) is)
    inst (SEL  t f) = "SEL "  ++ t ++ " " ++ f
    inst (TSEL t f) = "TSEL " ++ t ++ " " ++ f
    inst (LDF f)    = "LDF "  ++ f
    inst i          = show i

prettyProgram :: Program -> String
prettyProgram p = (unlines . map line . assocs) p
  where
    k = (length . show . snd . bounds) p
    line (addr, inst) = padLeft k (show addr) ++ ": " ++ show inst

padLeft :: Int -> String -> String
padLeft k s = replicate (k - length s) ' ' ++ s
