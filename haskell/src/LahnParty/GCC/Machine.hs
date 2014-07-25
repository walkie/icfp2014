
module LahnParty.GCC.Machine where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Strict

import LahnParty.GCC.Syntax
import LahnParty.GCC.State

-- | Errors that can occur during execution of a GCC program.
data Error = EnvError   EnvError
           | StackError StackError

-- | GCC execution monad.
type GCCM m a = StateT GCC (ExceptT Error m) a

-- | Increment the program counter.
incPC :: Monad m => GCCM m ()
incPC = pc += 1

-- | Set the program counter.
setPC :: Monad m => Addr -> GCCM m ()
setPC a = pc .= a

-- | Push value to the data stack.
pushD :: Monad m => Value -> GCCM m ()
pushD v = stackD %= (v:)

-- | Push address to the control stack.
pushC :: Monad m => Addr -> GCCM m ()
pushC a = stackC %= (a:)

-- | Pop value from the data stack.
popD :: Monad m => GCCM m Value
popD = do s <- use stackD
          case s of
            (v:vs) -> stackD .= vs >> return v
            _      -> throwError (StackError EmptyDataStack)
       
-- | Pop address from the control stack.
popC :: Monad m => GCCM m Addr
popC = do s <- use stackC
          case s of
            (a:as) -> stackC .= as >> return a
            _      -> throwError (StackError EmptyControlStack)
