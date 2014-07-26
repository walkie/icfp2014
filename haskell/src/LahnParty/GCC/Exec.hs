
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


--
-- * Program execution
--

-- | Run a program from start to stop, returning the resulting state and
--   any error that occured.
runProgramT :: Monad m => Program -> m (Maybe Error, GCC)
runProgramT p = liftM maybeError $ runStateT (runExceptT (untilStop p)) initGCC
  where maybeError = over _1 (either Just (const Nothing))

-- | Run a program from start to stop, returning the resulting state and
--   any error that occured.
runProgram :: Program -> (Maybe Error, GCC)
runProgram = runIdentity . runProgramT

-- | Execute one step of a program.
step :: Monad m => Program -> GCCM m ()
step p = do
  i <- use pc
  unless (inRange (bounds p) i) $ throwError IllegalPC
  execInst (p ! i)
  
-- | Run a program until it stops.
untilStop :: Monad m => Program -> GCCM m ()
untilStop p = do stop <- isStop; unless stop (step p)


--
-- * Instruction execution
--

-- | Apply a function to the first two integers popped off the data stack,
--   pushing the result.
intOp :: Monad m => (Int -> Int -> Int) -> GCCM m ()
intOp f = do a <- popInt; b <- popInt; pushD (Lit (f a b))

-- | Apply a function to the first two integers popped off the data stack,
--   push 1 if the result is true, otherwise 0.
boolOp :: Monad m => (Int -> Int -> Bool) -> GCCM m ()
boolOp f = intOp (\a b -> if f a b then 1 else 0)

-- | Execute a single instruction.
execInst :: Monad m => Inst -> GCCM m ()

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
execInst CONS = liftM2 Pair popD popD >> incPC
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
  pushFramePtr
  pushReturn
  env .= Values (reverse args) : fenv
  pc  .= faddr

execInst RTN = do
  stop <- isStop
  unless stop $ do
    raddr <- popReturn
    renv  <- popFramePtr
    env .= renv
    pc  .= raddr

-- Recursive environment function calls
execInst (DUM n) = env %= (Dummy n :) >> incPC

execInst (RAP n) = do
  (faddr,fenv) <- popClos
  popDummy n
  args <- replicateM n popD
  env %= (Values (reverse args) :)
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
  pushFramePtr
  env .= Values (reverse args) : fenv
  pc  .= faddr

execInst (TRAP n) = do
  (faddr,fenv) <- popClos
  popDummy n
  args <- replicateM n popD
  env %= (Values (reverse args) :)
  pushFramePtr
  env .= fenv
  pc  .= faddr
  
-- Pascal extensions
execInst (ST n i) = popD >>= envSet n i >> incPC
  
-- Debug extensions
execInst DBUG = throwError $ Unimplemented "DBUG"
execInst BRK  = throwError $ Unimplemented "BRK"
