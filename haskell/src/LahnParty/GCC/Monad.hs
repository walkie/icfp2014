
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
data Error
  =  EnvError     EnvError
  |  StackError   StackError
  |  TypeError    String
  |  ControlError String
  |  DivByZero
  deriving (Eq,Show)

-- | Convert the given value to an integer, or raise a type error.
toInt :: Monad m => Value -> GCCM m Int
toInt (Lit i) = return i
toInt v = throwError $ TypeError $ "Expected int, got: " ++ show v

-- | Convert the given value to a pair, or raise a type error.
toPair :: Monad m => Value -> GCCM m (Value,Value)
toPair (Pair a b) = return (a,b)
toPair v = throwError $ TypeError $ "Expected pair, got: " ++ show v

-- | Convert the given value to a closure, or raise a type error.
toClos :: Monad m => Value -> GCCM m (Addr,Env)
toClos (Clos a e) = return (a,e)
toClos v = throwError $ TypeError $ "Expected closure, got: " ++ show v

-- | Convert the control element to a join address.
toJoin :: Monad m => Control -> GCCM m Addr
toJoin (Join a) = return a
toJoin c = throwError $ ControlError $ "Expected join, got: " ++ show c

-- | Convert the control element to a return address.
toReturn :: Monad m => Control -> GCCM m Addr
toReturn (Return a) = return a
toReturn c = throwError $ ControlError $ "Expected return, got: " ++ show c

-- | Convert the control element to a frame pointer (environment).
toFramePtr :: Monad m => Control -> GCCM m Env
toFramePtr (FramePtr e) = return e
toFramePtr c = throwError $ ControlError $ "Expected frame pointer, got: " ++ show c


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
popClos :: Monad m => GCCM m (Addr,Env)
popClos = popD >>= toClos
       

-- ** Manipulate control stack

-- | Push address to the control stack.
pushC :: Monad m => Control -> GCCM m ()
pushC a = stackC %= (a:)

-- | Pop address from the control stack.
popC :: Monad m => GCCM m Control
popC = do
  s <- use stackC
  case s of
    (a:as) -> stackC .= as >> return a
    _      -> throwError (StackError EmptyControlStack)

-- | Pop a join address off the control stack.
popJoin :: Monad m => GCCM m Addr
popJoin = popC >>= toJoin

-- | Pop a return address off the control stack.
popReturn :: Monad m => GCCM m Addr
popReturn = popC >>= toReturn

-- | Pop a frame pointer off the control stack.
popFramePtr :: Monad m => GCCM m Env
popFramePtr = popC >>= toFramePtr


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
