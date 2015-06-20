import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative (Applicative(..))

newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

bindEitherT :: (Monad m) => EitherT a m b -> (b -> EitherT a m c) -> EitherT a m c
bindEitherT x f = EitherT $ do
  ret <- runEitherT x
  case ret of
    Left err -> return $ Left err
    Right y -> runEitherT (f y)

returnEitherT :: (Monad m) => b -> EitherT a m b
returnEitherT b = EitherT $ return (Right b)

instance (Monad m) => Monad (EitherT a m) where
  x >>= f = bindEitherT x f
  return  = returnEitherT

instance (Monad m) => Functor (EitherT a m) where
  fmap = liftM

instance (Monad m) => Applicative (EitherT a m) where
  pure  = return
  (<*>) = ap
  

liftEitherT :: (Monad m) => m b -> EitherT a m b
liftEitherT m = EitherT (Right `liftM` m)

instance MonadTrans (EitherT a) where
  lift = liftEitherT

lget = lift get
lput = lift . put

lock :: EitherT String (State Bool) ()
lock = do
  lput True
  
unlock :: EitherT String (State Bool) ()
unlock = do
  lput False

calcAdd :: Int -> Int -> EitherT String (State Bool) Int
calcAdd x y = do
  st <- lget
  if st
  then do
    EitherT $ return $ Left "State is locked! cannot do computation"
  else do
    lock
    return $ x + y

runCalculations :: EitherT String (State Bool) Int
runCalculations = do
  unlock
  n <- calcAdd 2 3
  -- unlock
  n2 <- calcAdd 4 1
  return $ n * n2
  
main = do
  let res = evalState (runEitherT runCalculations) True
  print res
