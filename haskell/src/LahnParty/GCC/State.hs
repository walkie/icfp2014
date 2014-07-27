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
  |  Clos Addr  FramePtr
  deriving (Eq,Read,Show)

-- | Elements in the control stack.
data Control
  =  Join     Addr
  |  Return   Addr
  |  FramePtr FramePtr
  |  Stop
  deriving (Eq,Read,Show)


--
-- * Environment
--

-- | A frame pointer is an index into the list of environment frames.
type FramePtr = Int

-- | An environment frame contains a pointer to its parent and some content.
data Frame = Frame {
  _parent  :: Addr,
  _content :: Content
} deriving (Eq,Read,Show)
  
-- | An environment frame contains a fixed-length list of slots for values.
--   A dummy frame is an uninitialized environment frame.
data Content
  =  Values [Value]
  |  Dummy  Int
  deriving (Eq,Read,Show)

$(makeLenses ''Frame)

--
-- * Program state
--

-- | State of the GCC machine.
data GCC = GCC {
  _pc     :: Addr,
  _stackD :: DataStack,
  _stackC :: ControlStack,
  _frames :: [Frame],
  _env    :: FramePtr
} deriving (Eq,Read,Show)

initGCC :: GCC
initGCC = GCC 0 [] [] [] (-1)

$(makeLenses ''GCC)
