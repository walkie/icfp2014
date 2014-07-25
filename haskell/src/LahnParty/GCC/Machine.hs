
module LahnParty.GCC.Machine where

import Control.Monad.Except
import Control.Monad.State.Strict

import LahnParty.GCC.Syntax
import LahnParty.GCC.State

-- | Errors that can occur during execution of a GCC program.
data Error = EnvError EnvError

-- | GCC execution monad.
type GCCM m a = StateT GCC (ExceptT Error m) a
