
module Main where

import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit

import LahnParty.GCC


-- ** Command line options

data Opt = Symbolic | Trace | Help
  deriving (Eq,Show)

opts =
  [ Option ['s'] ["symbolic"] (NoArg Symbolic)
      "program uses symbolic names rather than addresses"
  , Option ['t'] ["trace"]    (NoArg Trace)
      "print trace of machine states during execution"
  , Option ['h'] ["help"]     (NoArg Help)
      "print this help message and exit"
  ]

usage :: String
usage = usageInfo "Run a GCC program provided on STDIN." opts

readOpts :: IO [Opt]
readOpts = do
    args <- getArgs
    case getOpt Permute opts args of
      (os,[],[]) -> return os
      (_ ,_ ,es) -> ioError (userError (concat es ++ usage))


-- ** Read program from STDIN

readAssembly :: IO Assembly
readAssembly = getContents >>= return . read

readProgram :: IO Program
readProgram = getContents >>= return . program . read


-- ** Main

main = do
  os <- readOpts
  when (elem Help os) (putStr usage >> exitSuccess)
  p  <- if elem Symbolic os
          then readAssembly >>= return . assemble
          else readProgram
  if elem Trace os
    then traceProgram p
    else putStrLn (prettyResult p (runProgram p))
