
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
prettyResult :: Program -> Either Error GCC -> String
prettyResult p = either show (prettyGCC p)

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
prettyValue (Clos f e) = "[" ++ show f ++ ", " ++ show e ++ "]"

-- | Pretty print a control element.
prettyControl :: Control -> String
prettyControl (Join     a) = "Join "     ++ show a
prettyControl (Return   a) = "Return "   ++ show a
prettyControl (FramePtr a) = "FramePtr " ++ show a
prettyControl Stop         = "Stop"

-- | Pretty print an environment frame.
prettyFrame :: (Int,Frame) -> String
prettyFrame (n, Frame p c) =
    unlines $ ("Frame " ++ show n ++ " => " ++ show p) : case c of
      Dummy m   -> ["[Dummy " ++ show m ++ "]"]
      Values vs -> let k = length (show (length vs))
                   in map (numberedLine (k + 2) prettyValue) (zip [0..] vs)

-- | Pretty print the list of frames.
prettyFrames :: [Frame] -> String
prettyFrames = unlines . map prettyFrame . zip [0..]

-- | Pretty print a program execution state.
prettyGCC :: Program -> GCC -> String
prettyGCC p (GCC pc ds cs fs fp) = unlines 
    [ padLeft k (show pc) ++ ": " ++ show (p ! pc), ""
    , "== Data Stack =="
    , prettyStackD ds
    , "== Control Stack =="
    , prettyStackC cs
    , "== Environment =="
    , "FP: " ++ show fp
    , prettyFrames fs ]
  where k = (length . show . snd . bounds) p

-- | Format a numbered line.
numberedLine :: Int -> (a -> String) -> (Int, a) -> String
numberedLine k f (i,a) = padLeft k (show i) ++ ": " ++ f a

-- | Pad a string to the given length by adding spaces to the left.
padLeft :: Int -> String -> String
padLeft k s = replicate (k - length s) ' ' ++ s
