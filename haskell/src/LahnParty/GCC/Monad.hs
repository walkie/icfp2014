
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

-- | Run a computation in the execution monad.
runGCCM :: Monad m => GCCM m a -> GCC -> m (Either Error GCC)
runGCCM m s = runExceptT (execStateT m s)

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
popClos :: Monad m => GCCM m (Addr,FramePtr)
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

-- | True if the control stack is empty.
isEmptyC :: Monad m => GCCM m Bool
isEmptyC = liftM null (use stackC)

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
popFramePtr :: Monad m => GCCM m FramePtr
popFramePtr = popC >>= toFramePtr


-- ** Manipulate environment

-- | Lookup a frame pointer.
frame :: Monad m => FramePtr -> GCCM m Frame
frame fp = do
  fs <- use frames
  unless (fp >= 0 && fp < length fs) $
    throwError $ FrameError ("no frame at fp: " ++ show fp)
  return (reverse fs !! fp)

-- | Build the current environment.
buildEnv :: Monad m => GCCM m [Frame]
buildEnv = do use env >>= go
  where 
    go (-1) = return []
    go fp   = do f <- frame fp; liftM (f:) (go (_parent f))

-- | Get a value from the environment.
envGet :: Monad m => Int -> Int -> GCCM m Value
envGet n i = buildEnv >>= getAt n >>= toValues >>= getAt i

-- | Set a value in the environment.
envSet :: Monad m => Int -> Int -> Value -> GCCM m ()
envSet n i v = do
    fp <- if n == 0 then use env
          else buildEnv >>= getAt (n-1) >>= (return . _parent)
    frames <~ (use frames >>= updateAt fp inFrame)
  where
    inFrame f = do
      vs <- toValues f >>= setAt i v;
      return $ set content (Values vs) f

-- | Create a new environment frame and get a pointer to it.
newFrame :: Monad m => FramePtr -> Content -> GCCM m FramePtr
newFrame fp c = do
  frames %= (Frame fp c :)
  liftM (subtract 1 . length) (use frames)

-- | Replace a dummy frame at the indicated frame pointer.
replaceDummy :: Monad m => FramePtr -> [Value] -> GCCM m ()
replaceDummy fp vs = do
    frames <~ (use frames >>= updateAt fp replace)
  where
    n = length vs
    replace f = do
      n' <- toDummy f
      if n == n' then return $ set content (Values vs) f
      else raise FrameError ("dummy length " ++ show n) (show n')
