
module LahnParty.GCC.Exec where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Array

import LahnParty.GCC.Syntax
import LahnParty.GCC.State
import LahnParty.GCC.Error
import LahnParty.GCC.Monad
import LahnParty.GCC.Pretty


--
-- * Program execution
--

-- | Run a program from start to stop, returning the resulting state and
--   any error that occured.
runProgramT :: Monad m => Program -> m (Either Error GCC)
runProgramT p = runGCCM (untilStop (step p)) initGCC

-- | Run a program from start to stop, returning the resulting state and
--   any error that occured.
runProgram :: Program -> Either Error GCC
runProgram = runIdentity . runProgramT

-- | Run a program from start to stop, printing the state at each step.
traceProgram :: Program -> IO ()
traceProgram p = do
  putStrLn "== Program =="
  putStrLn (prettyProgram p)
  res <- runGCCM (trace p) initGCC
  putStrLn (replicate 40 '-')
  putStrLn (prettyResult p res)

-- | Execute one step of a program.
step :: Monad m => Program -> GCCM m ()
step p = do
  i <- use pc
  unless (inRange (bounds p) i) $ throwError IllegalPC
  execInst (p ! i)
  
-- | Run a program until it stops.
untilStop :: Monad m => GCCM m () -> GCCM m ()
untilStop m = do stop <- isStop; unless stop (m >> untilStop m)

-- | Run a program until it stops, printing the state at each step.
trace :: Program -> GCCM IO ()
trace p = untilStop $ do
            i <- use pc
            unless (inRange (bounds p) i) $ throwError IllegalPC
            liftIO $ putStrLn (replicate 40 '-')
            get >>= liftIO . putStr . prettyGCC p
            step p


--
-- * Instruction execution
--

-- | Apply a function to the first two integers popped off the data stack,
--   pushing the result.
intOp :: Monad m => (Int -> Int -> Int) -> GCCM m ()
intOp f = do b <- popInt; a <- popInt; pushD (Lit (f a b))

-- | Apply a function to the first two integers popped off the data stack,
--   push 1 if the result is true, otherwise 0.
boolOp :: Monad m => (Int -> Int -> Bool) -> GCCM m ()
boolOp f = intOp (\a b -> if f a b then 1 else 0)

-- | Execute a single instruction.
execInst :: Monad m => Inst Addr -> GCCM m ()

-- Load
execInst (LDC n)  = pushD (Lit n) >> incPC
execInst (LD n i) = envGet n i >>= pushD >> incPC

-- Arithmetic
execInst ADD = intOp (+) >> incPC
execInst SUB = intOp (-) >> incPC
execInst MUL = intOp (*) >> incPC
execInst DIV = do
  b <- popInt; a <- popInt;
  when (b == 0) (throwError DivByZero)
  pushD (Lit (a `div` b))
  incPC

-- Logic
execInst CEQ  = boolOp (==) >> incPC
execInst CGT  = boolOp (>)  >> incPC
execInst CGTE = boolOp (>=) >> incPC
execInst ATOM = do
  v <- popD
  case v of
    Lit _ -> pushD (Lit 1)
    _     -> pushD (Lit 0)
  incPC

-- Pairs
execInst CONS = do b <- popD; a <- popD; pushD (Pair a b); incPC
execInst CAR  = popPair >>= pushD . fst >> incPC
execInst CDR  = popPair >>= pushD . snd >> incPC

-- Branching
execInst JOIN      = popJoin >>= setPC
execInst (SEL t f) = do
  b <- popInt
  pushJoin
  if b == 0
    then pc .= f
    else pc .= t

-- Function calls
execInst (LDF a) = use env >>= pushD . Clos a >> incPC

execInst (AP n) = do
  (faddr,fenv) <- popClos
  args <- replicateM n popD
  fp   <- newFrame fenv (Values (reverse args))
  pushFramePtr
  pushReturn
  env .= fp
  pc  .= faddr

execInst RTN = do
  stop <- isEmptyC
  if stop
    then pushC Stop
    else do
      raddr <- popReturn
      renv  <- popFramePtr
      env .= renv
      pc  .= raddr

-- Recursive environment function calls
execInst (DUM n) = do
  old <- use env
  new <- newFrame old (Dummy n)
  env .= new
  incPC

execInst (RAP n) = do
  (faddr,fenv) <- popClos
  fp   <- use env
  args <- replicateM n popD
  replaceDummy fp (reverse args)
  pushFramePtr
  pushReturn
  env .= fenv
  pc  .= faddr

-- Terminate execution (deprecated)
execInst STOP = pushC Stop
  
-- Tail call extensions
execInst (TSEL t f) = do
  b <- popInt
  if b == 0
    then pc .= f
    else pc .= t

execInst (TAP n) = do
  (faddr,fenv) <- popClos
  args <- replicateM n popD
  fp   <- newFrame fenv (Values (reverse args))
  pushFramePtr
  env .= fp
  pc  .= faddr

execInst (TRAP n) = do
  (faddr,fenv) <- popClos
  fp   <- use env
  args <- replicateM n popD
  replaceDummy fp (reverse args)
  pushFramePtr
  env .= fenv
  pc  .= faddr
  
-- Pascal extensions
execInst (ST n i) = popD >>= envSet n i >> incPC
  
-- Debug extensions
execInst DBUG = throwError $ Unimplemented "DBUG"
execInst BRK  = throwError $ Unimplemented "BRK"
