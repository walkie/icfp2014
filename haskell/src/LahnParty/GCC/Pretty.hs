
module LahnParty.GCC.Pretty where

import Data.Array
import Data.List (intercalate)

import LahnParty.GCC.Assembler
import LahnParty.GCC.State
import LahnParty.GCC.Syntax
import LahnParty.GCC.Error


prettyAssembly :: Assembly -> String
prettyAssembly p = unlines (map block p)
  where
    block (l,is) = unlines ((l ++ ":") : map (("  " ++) . inst) is)
    inst (SEL  t f) = "SEL "  ++ t ++ " " ++ f
    inst (TSEL t f) = "TSEL " ++ t ++ " " ++ f
    inst (LDF f)    = "LDF "  ++ f
    inst i          = show i

prettyProgram :: Program -> String
prettyProgram p = unlines (map (numberedLine k show) (assocs p))
  where k = (length . show . snd . bounds) p

prettyResult :: Either Error GCC -> String
prettyResult = either show prettyGCC

prettyStack :: (a -> String) -> Stack a -> String
prettyStack f s = unlines (map (numberedLine k f) (zip [0..] s))
  where k = length (show (length s))

prettyStackD :: DataStack -> String
prettyStackD = prettyStack prettyValue

prettyStackC :: ControlStack -> String
prettyStackC = prettyStack prettyControl

prettyValue :: Value -> String
prettyValue (Lit i)    = show i
prettyValue (Pair a b) = "(" ++ prettyValue a ++ ", " ++ prettyValue b ++ ")"
prettyValue (Clos f m) = show f ++ " " ++ prettyEnv 2 m

prettyControl :: Control -> String
prettyControl (Join     a) = "Join "   ++ show a
prettyControl (Return   a) = "Return " ++ show a
prettyControl (FramePtr e) = "FramePtr" ++ 
                             if null e then " [Empty]" else prettyEnv 8 e
prettyControl Stop         = "Stop"

prettyFrame :: Int -> (Int,Frame) -> String
prettyFrame k (n,f) = unlines $ padLeft k ("Frame " ++ show n) : go f
  where
    go (Dummy m)   = ["  [Dummy " ++ show m ++ "]"]
    go (Values vs) = map (numberedLine (k + k' + 2) prettyValue) (zip [0..] vs)
      where k' = length (show (length vs))

prettyEnv :: Int -> Env -> String
prettyEnv k = unlines . map (prettyFrame k) . zip [0..]

prettyGCC :: GCC -> String
prettyGCC (GCC pc ds cs env) = unlines 
  ["PC: " ++ show pc, "",
   "== Data Stack ==",    prettyStackD ds,
   "== Control Stack ==", prettyStackC cs,
   "== Environment ==",   prettyEnv 0 env]

numberedLine :: Int -> (a -> String) -> (Int, a) -> String
numberedLine k f (i,a) = padLeft k (show i) ++ ": " ++ f a

padLeft :: Int -> String -> String
padLeft k s = replicate (k - length s) ' ' ++ s
