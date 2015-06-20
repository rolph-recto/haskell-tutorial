import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

bindMaybeT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMaybeT x f = MaybeT $ do
  ret <- runMaybeT x
  case ret of
    Nothing -> return Nothing
    Just y -> runMaybeT (f y)

returnMaybeT :: (Monad m) => a -> MaybeT m a
returnMaybeT a = MaybeT $ return (Just a)

failMaybeT :: (Monad m) => t -> MaybeT m a
failMaybeT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
  x >>= f   = bindMaybeT x f
  return    = returnMaybeT
  fail      = failMaybeT

liftMaybeT :: (Monad m) => m a -> MaybeT m a
liftMaybeT m = MaybeT (Just `liftM` m)

instance MonadTrans MaybeT where
  lift = liftMaybeT

lget = lift get
lput = lift . put

unlock :: MaybeT (State String) ()
unlock = do
  lput "unlock"

lock :: MaybeT (State String) ()
lock = do
  lput "lock"

calcAdd :: Int -> Int -> MaybeT (State String) Int
calcAdd x y = do
  st <- lget
  if st == "unlock"
  then do
    return $ x + y
  else do
    fail "no good!"

runCalculations :: MaybeT (State String) Int
runCalculations = do
  -- unlock
  n <- calcAdd 2 3
  unlock
  n2 <- calcAdd 1 4
  return $ n * n2
  

main = do
  let res = evalState (runMaybeT runCalculations) "lock"
  print res
  return ()
