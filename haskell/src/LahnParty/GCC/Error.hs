{-# LANGUAGE FlexibleContexts #-}

module LahnParty.GCC.Error where

import Control.Monad.Except

import LahnParty.GCC.Syntax
import LahnParty.GCC.State


--
-- * Execution errors
--

-- | Errors that can occur during execution of a GCC program.
data Error
  =  EnvError     EnvError
  |  StackError   StackError
  |  TypeError    String
  |  ControlError String
  |  DivByZero
  deriving (Eq,Show)

-- | Raise an error corresponding to an unmet expectation.
raise :: (MonadError Error m, Show x) => (String -> Error) -> String -> x -> m a
raise err exp got = throwError $ err ("Expected " ++ exp ++ ", got: " ++ show got)

-- | Get the value as an integer, or raise a type error.
toInt :: MonadError Error m => Value -> m Int
toInt (Lit i) = return i
toInt v       = raise TypeError "int" v

-- | Get the value as a pair, or raise a type error.
toPair :: MonadError Error m => Value -> m (Value,Value)
toPair (Pair a b) = return (a,b)
toPair v          = raise TypeError "pair" v

-- | Get the value as a closure, or raise a type error.
toClos :: MonadError Error m => Value -> m (Addr,Env)
toClos (Clos a e) = return (a,e)
toClos v          = raise TypeError "closure" v

-- | Get the control element as a join address, or raise a control error.
toJoin :: MonadError Error m => Control -> m Addr
toJoin (Join a) = return a
toJoin c        = raise ControlError "join" c

-- | Get the control element as a return address, or raise a control error.
toReturn :: MonadError Error m => Control -> m Addr
toReturn (Return a) = return a
toReturn c          = raise ControlError "return" c

-- | Get the control element as a frame pointer (environment),
--   or raise a control error.
toFramePtr :: MonadError Error m => Control -> m Env
toFramePtr (FramePtr e) = return e
toFramePtr c            = raise ControlError "frame pointer" c
