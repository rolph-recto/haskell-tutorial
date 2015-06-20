import System.IO
import Control.Monad.State

addN :: Int -> StateT Int IO String
addN n = do
  val <- get
  put $ val + n
  return $ "adding " ++ (show n)

subN :: Int -> StateT Int IO String
subN n = do
  val <- get
  put $ val + n
  return $ "subtracting " ++ (show n)

report :: StateT Int IO String -> StateT Int IO String
report action = do
  log <- action
  lift $ putStrLn log
  return log

sequence1 :: StateT Int IO ()
sequence1 = do
  report $ addN 1
  report $ subN 2
  report $ addN 5
  report $ addN 9
  report $ subN 10
  return ()

main = do 
  v <- execStateT sequence1 0
  print v
