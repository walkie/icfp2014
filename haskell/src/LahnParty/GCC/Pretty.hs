
module LahnParty.GCC.Pretty where

import Data.Array
import Data.List (intercalate)

import LahnParty.GCC.Assembler
import LahnParty.GCC.State
import LahnParty.GCC.Syntax
import LahnParty.GCC.Error


-- | Pretty print an assembly program.
prettyAssembly :: Assembly -> String
prettyAssembly p = unlines (map block p)
  where
    block (l,is) = unlines ((l ++ ":") : map (("  " ++) . inst) is)
    inst (SEL  t f) = "SEL "  ++ t ++ " " ++ f
    inst (TSEL t f) = "TSEL " ++ t ++ " " ++ f
    inst (LDF f)    = "LDF "  ++ f
    inst i          = show i

-- | Pretty print an assembled program.
prettyProgram :: Program -> String
prettyProgram p = unlines (map (numberedLine k show) (assocs p))
  where k = (length . show . snd . bounds) p

-- | Pretty print the result of a program execution.
prettyResult :: Either Error GCC -> String
prettyResult = either show prettyGCC

-- | Pretty print a stack using the given function for pretty printing its
--   elements.
prettyStack :: (a -> String) -> Stack a -> String
prettyStack f s = unlines (map (numberedLine k f) (zip [0..] s))
  where k = length (show (length s))

-- | Pretty print the data stack.
prettyStackD :: DataStack -> String
prettyStackD = prettyStack prettyValue

-- | Pretty print the control stack.
prettyStackC :: ControlStack -> String
prettyStackC = prettyStack prettyControl

-- | Pretty print a value.
prettyValue :: Value -> String
prettyValue (Lit i)    = show i
prettyValue (Pair a b) = "(" ++ prettyValue a ++ ", " ++ prettyValue b ++ ")"
prettyValue (Clos f m) = show f ++ " " ++ prettyEnv 2 m

-- | Pretty print a control element.
prettyControl :: Control -> String
prettyControl (Join     a) = "Join "   ++ show a
prettyControl (Return   a) = "Return " ++ show a
prettyControl (FramePtr e) = "FramePtr" ++ 
                             if null e then " [Empty]" else prettyEnv 8 e
prettyControl Stop         = "Stop"

-- | Pretty print a control element.
prettyFrame :: Int -> (Int,Frame) -> String
prettyFrame k (n,f) = unlines $ padLeft k ("Frame " ++ show n) : go f
  where
    go (Dummy m)   = ["  [Dummy " ++ show m ++ "]"]
    go (Values vs) = map (numberedLine (k + k' + 2) prettyValue) (zip [0..] vs)
      where k' = length (show (length vs))

-- | Pretty print an environment.
prettyEnv :: Int -> Env -> String
prettyEnv k = unlines . map (prettyFrame k) . zip [0..]

-- | Pretty print a program execution state.
prettyGCC :: GCC -> String
prettyGCC (GCC pc ds cs env) = unlines 
  ["PC: " ++ show pc, "",
   "== Data Stack ==",    prettyStackD ds,
   "== Control Stack ==", prettyStackC cs,
   "== Environment ==",   prettyEnv 0 env]

-- | Format a numbered line.
numberedLine :: Int -> (a -> String) -> (Int, a) -> String
numberedLine k f (i,a) = padLeft k (show i) ++ ": " ++ f a

-- | Pad a string to the given length by adding spaces to the left.
padLeft :: Int -> String -> String
padLeft k s = replicate (k - length s) ' ' ++ s
