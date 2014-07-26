
module Main where

import System.Console.GetOpt
import System.Environment

import LahnParty.GCC


-- ** Command line options

data Opt = Symbolic | Trace
  deriving (Eq,Show)

opts =
  [ Option ['s'] ["symbolic"] (NoArg Symbolic)
      "program uses symbolic names rather than addresses"
  , Option ['t'] ["trace"]    (NoArg Trace)
      "print trace of machine states during execution"
  ]

readOpts :: IO [Opt]
readOpts = do
    args <- getArgs
    case getOpt Permute opts args of
      (os,[],[]) -> return os
      (_ ,_ ,es) -> ioError (userError (concat es ++ usageInfo head opts))
  where
    head = "Run a GCC program provided on STDIN."


-- ** Read program from STDIN

readAssembly :: IO Assembly
readAssembly = getContents >>= return . read

readProgram :: IO Program
readProgram = getContents >>= return . program . read


-- ** Main

main = do
  os <- readOpts
  p  <- if elem Symbolic os
          then readAssembly >>= return . assemble
          else readProgram
  if elem Trace os
    then traceProgram p
    else putStrLn (prettyResult p (runProgram p))
