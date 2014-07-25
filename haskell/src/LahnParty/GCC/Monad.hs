
module LahnParty.GCC.Monad where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Strict

import LahnParty.GCC.Syntax
import LahnParty.GCC.State


--
-- * Execution monad
--

-- | GCC execution monad.
type GCCM m a = StateT GCC (ExceptT Error m) a


-- ** Errors

-- | Errors that can occur during execution of a GCC program.
data Error = EnvError   EnvError
           | StackError StackError
           | TypeError  String
           | DivByZero

-- | Convert the given value to an integer, or raise a type error.
toInt :: Monad m => Value -> GCCM m Int
toInt (Lit i) = return i
toInt v = throwError $ TypeError $ "Expected int, got: " ++ show v

-- | Convert the given value to a pair, or raise a type error.
toPair :: Monad m => Value -> GCCM m (Value,Value)
toPair (Pair a b) = return (a,b)
toPair v = throwError $ TypeError $ "Expected pair, got: " ++ show v

-- | Convert the given value to a closure, or raise a type error.
toClos :: Monad m => Value -> GCCM m (Frame,Addr)
toClos (Clos f a) = return (f,a)
toClos v = throwError $ TypeError $ "Expected closure, got: " ++ show v


-- ** Manipulate program counter

-- | Increment the program counter.
incPC :: Monad m => GCCM m ()
incPC = pc += 1

-- | Set the program counter.
setPC :: Monad m => Addr -> GCCM m ()
setPC a = pc .= a


-- ** Manipulate data stack

-- | Push value to the data stack.
pushD :: Monad m => Value -> GCCM m ()
pushD v = stackD %= (v:)

-- | Pop value from the data stack.
popD :: Monad m => GCCM m Value
popD = do 
  s <- use stackD
  case s of
    (v:vs) -> stackD .= vs >> return v
    _      -> throwError (StackError EmptyDataStack)

-- | Pop an integer off the data stack.
popInt :: Monad m => GCCM m Int
popInt = popD >>= toInt

-- | Pop a pair off the data stack.
popPair :: Monad m => GCCM m (Value,Value)
popPair = popD >>= toPair

-- | Pop a closure off the data stack.
popClos :: Monad m => GCCM m (Frame,Addr)
popClos = popD >>= toClos
       

-- ** Manipulate control stack

-- | Push address to the control stack.
pushC :: Monad m => Addr -> GCCM m ()
pushC a = stackC %= (a:)

-- | Pop address from the control stack.
popC :: Monad m => GCCM m Addr
popC = do
  s <- use stackC
  case s of
    (a:as) -> stackC .= as >> return a
    _      -> throwError (StackError EmptyControlStack)


-- ** Manipulate environment

{-
-- | Traversal to access a particular slot in the environment.
slot :: Applicative f => Int -> Int -> (Maybe Value -> f (Maybe Value)) -> GCC -> f GCC
slot n i = env . ix n . ix i
-}

-- | Get a value from the environment.
envGet :: Monad m => Int -> Int -> GCCM m Value
envGet n i = do
  e <- use env
  case _envGet n i e of
    Right v  -> return v
    Left err -> throwError (EnvError err)

-- | Set a value in the environment.
envSet :: Monad m => Int -> Int -> Value -> GCCM m ()
envSet n i v = do
  e <- use env
  case _envSet n i v e of
    Right e' -> env .= e'
    Left err -> throwError (EnvError err)
