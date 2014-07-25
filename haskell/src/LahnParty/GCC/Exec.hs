
module LahnParty.GCC.Exec where

import Control.Lens
import Control.Monad.Except

import LahnParty.GCC.Syntax
import LahnParty.GCC.State
import LahnParty.GCC.Monad


--
-- * Program execution
--

-- | Execute a single instruction.
inst :: Monad m => Inst -> GCCM m ()

inst (LDC n) = pushD (Lit n) >> incPC
