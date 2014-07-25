{-# LANGUAGE TemplateHaskell #-}

module LahnParty.GCC.State where

import Control.Lens
import Data.List (splitAt)

import LahnParty.GCC.Syntax


--
-- * Stacks
--

-- | A stack.
type Stack a = [a]

-- | The data stack.
type DataStack = Stack Value

-- | The control stack.
type ControlStack = Stack Control

-- | Values in the data stack or environment frame.
data Value 
  =  Lit  Int
  |  Pair Value Value
  |  Clos Addr  Env
  deriving (Eq,Show)

-- | Elements in the control stack.
data Control
  =  Join     Addr
  |  Return   Addr
  |  FramePtr Env
  |  Stop
  deriving (Eq,Show)


--
-- * Environment
--

-- | An environment frame is a fixed-length list of slots for values.
--   A dummy frame is an uninitialized environment frame.
data Frame = Values [Value]
           | Dummy  Int
  deriving (Eq,Show)

-- | An environment is a list of frames.
type Env = [Frame]


--
-- * Program state
--

-- | State of the GCC machine.
data GCC = GCC {
  _pc     :: Addr,
  _stackD :: DataStack,
  _stackC :: ControlStack,
  _env    :: Env
}

$(makeLenses ''GCC)
