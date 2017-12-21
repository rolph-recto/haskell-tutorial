{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Free
import qualified Data.Map.Strict as M

data GoVar = GoVar Int deriving (Show, Eq, Ord)

data GoExpr =
    GoInt Int
  | GoBool Bool
  | GoString String
  | GoVarExpr GoVar
  deriving (Show, Eq)

class GoExprable a where
  toGoExpr :: a -> GoExpr
  
instance GoExprable Int where
  toGoExpr = GoInt

instance GoExprable Bool where
  toGoExpr = GoBool

instance GoExprable GoVar where
  toGoExpr = GoVarExpr

instance GoExprable GoExpr where
  toGoExpr = id

type GoRoutine = Int
type GoChan    = Int

data GoCmd next = 
    GoRun (GoCmd (Free GoCmd ())) next
  | GoMakeChan (GoChan -> next)
  | GoPutChan GoChan GoExpr next
  | GoGetChan GoChan (GoExpr -> next)
  | GoPrint String next
  | GoDone
  deriving (Functor)


liftFree x  = Free (fmap Pure x)

go (Free cmd) = liftFree (GoRun cmd ())
newchan       = liftFree (GoMakeChan id)
putchan c v   = liftFree (GoPutChan c (toGoExpr v) ())
getchan c     = liftFree (GoGetChan c id)
goprint s     = liftFree (GoPrint s ())
done          = liftFree GoDone

type GoProgram  = Free GoCmd

data GoRuntime =
  GoRuntime {
    curGoroutine :: GoRoutine,
    nextGoroutine :: GoRoutine,
    goroutines :: [(GoRoutine, GoProgram ())],
    nextChannel :: GoChan,
    channels :: M.Map GoChan (Maybe GoExpr)
  }
  
type GoInterp   = ExceptT String (StateT GoRuntime IO)

interpGo :: GoProgram () -> GoInterp ()
interpGo p = case p of
  Free (GoRun goroutine next) -> do
    st <- get
    let rnum = nextGoroutine st
    let rmap = goroutines st
    let rmap' = rmap ++ [(rnum, Free goroutine)]
    put $ st { nextGoroutine = rnum+1, goroutines = rmap' }
    interpGo next

  Free (GoMakeChan next) -> do
    st <- get
    let cnum = nextChannel st
    let cmap = channels st
    let cmap' = M.insert cnum Nothing cmap
    put $ st { nextChannel = cnum+1, channels = cmap' }
    interpGo $ next cnum

  Free (GoPutChan c v next) -> do
    st <- get
    let cmap = channels st

    -- block goroutine if channel is full, otherwise put value into channel
    case M.lookup c cmap of 
      Just Nothing -> do
        let cmap' = M.insert c (Just v) cmap
        put $ st { channels = cmap' }
        interpGo next

      Just _ -> do
        let rmap = goroutines st
        case rmap of
          (rid, cont):rs -> do
            let nextRoutines = rs ++ [(curGoroutine st, p)]
            put $ st { curGoroutine = rid, goroutines = nextRoutines }
            interpGo cont

          [] -> throwError "No goroutines to schedule!"

      Nothing -> throwError $ "Channel " ++ (show c) ++ " not found!"

  Free (GoGetChan c next) -> do
    st <- get
    let cmap = channels st

    -- block goroutine if channel is empty, otherwise get value from channel
    case M.lookup c cmap of 
      Just Nothing -> do
        let rmap = goroutines st
        case rmap of
          (rid, cont):rs -> do
            let nextRoutines = rs ++ [(curGoroutine st, p)]
            put $ st { curGoroutine = rid, goroutines = nextRoutines }
            interpGo cont

          [] -> throwError "No goroutines to schedule!"

      Just (Just cval) -> do
        let cmap' = M.insert c Nothing cmap
        put $ st { channels = cmap' }
        interpGo $ next cval

      Nothing -> throwError $ "Channel " ++ (show c) ++ " not found!"

  Free (GoPrint e next) -> do
    liftIO $ putStrLn e
    interpGo next

  Free GoDone -> do
    st <- get
    let rmap = goroutines st
    case rmap of
      (rid, cont):rs -> do
        put $ st { curGoroutine = rid, goroutines = rs }
        interpGo cont

      [] -> return ()

  -- goroutine is finished: run next one
  Pure r -> do
    st <- get
    let rmap = goroutines st
    case rmap of
      (rid, cont):rs -> do
        put $ st { curGoroutine = rid, goroutines = rs }
        interpGo cont

      [] -> return ()


run_gomain main = do
  let init = GoRuntime { curGoroutine = 0, nextGoroutine = 1,
              goroutines = [], nextChannel = 0, channels = M.empty }

  res <- evalStateT (runExceptT (interpGo main)) init
  case res of
    Left err  -> putStrLn err
    Right _   -> return ()


-- example: producer-consumer queue

consume :: GoChan -> GoProgram ()
consume chan = do
  v <- getchan chan
  goprint $ show v
  consume chan


produce :: [Int] -> GoChan -> GoProgram ()
produce range chan = do
  forM_ range $ \i -> do
    putchan chan i

  let offset  = length range

  if head range < 100
  then produce (map (+ offset) range) chan
  else done

bufmain :: GoProgram ()
bufmain = do
  c <- newchan
  go $ produce [1..10] c
  go $ consume c
  goprint "producer-consumer queue running!"



-- example: sieve of eratosthenes 

source :: Int -> GoChan -> GoProgram ()
source max c = do
  forM_ [2..max] $ \i -> do
    putchan c i


pfilter :: Int -> GoChan -> GoChan -> GoProgram ()
pfilter p left right = do
  GoInt v <- getchan left

  if v `mod` p /= 0
  then do
    putchan right v
    pfilter p left right

  else do
    pfilter p left right


sink :: GoChan -> GoProgram ()
sink left = do
  GoInt v <- getchan left
  goprint $ "got prime: " ++ (show v)
  right <- newchan
  go $ pfilter v left right
  sink right


sievemain = do
  goprint "running prime sieve..."
  c <- newchan
  go $ source 1000 c
  go $ sink c
          

