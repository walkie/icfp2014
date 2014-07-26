
module LahnParty.GCC.Monad where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Strict

import LahnParty.GCC.Syntax
import LahnParty.GCC.State
import LahnParty.GCC.Error


--
-- * Execution monad
--

-- | GCC execution monad.
type GCCM m a = StateT GCC (ExceptT Error m) a


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

-- | True if the top of the control stack is the Stop symbol.
isStop :: Monad m => GCCM m Bool
isStop = do
  s <- use stackC
  case s of
    (Stop:_) -> return True
    _        -> return False

-- | Push PC+1 to the control stack as a join address.
pushJoin :: Monad m => GCCM m ()
pushJoin = use pc >>= pushC . Join . (+1)

-- | Push PC+1 to the control stack as a return address.
pushReturn :: Monad m => GCCM m ()
pushReturn = use pc >>= pushC . Return . (+1)

-- | Push the current environment to the control stack.
pushFramePtr :: Monad m => GCCM m ()
pushFramePtr = use env >>= pushC . FramePtr

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

-- | Get a value from the environment.
envGet :: Monad m => Int -> Int -> GCCM m Value
envGet n i = use env >>= getAt n >>= toValues >>= getAt i

-- | Set a value in the environment.
envSet :: Monad m => Int -> Int -> Value -> GCCM m ()
envSet n i v = env <~ (use env >>= updateAt n inFrame)
  where inFrame f = toValues f >>= setAt i v >>= return . Values

-- | Pop a dummy frame of the size or raise an error.
popDummy :: Monad m => Int -> GCCM m ()
popDummy n = do
    e <- use env
    case e of
      (f:fs) -> toDummy f >>= check >> env .= fs
      _      -> throwError OutOfBounds
  where check n' = unless (n == n')
                 $ raise FrameError ("dummy length " ++ show n) (show n')
