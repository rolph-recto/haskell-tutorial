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

-- (in, out)
-- this allows the input and output channels to differ,
-- which allows us to implement channel combinators
type GoChan = (Int, Int) 

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
    nextGoroutine :: Int,
    goroutines :: [(GoRoutine, GoProgram ())],
    nextChannel :: Int,
    channels :: M.Map Int (Maybe GoExpr)
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
    interpGo $ next (cnum, cnum)

  Free (GoPutChan (c,_) v next) -> do
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

  Free (GoGetChan (_,c) next) -> do
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


-- channel combinators

-- pipe input from one channel and set it as the output of another channel
chan_pipe :: (GoChan -> GoChan -> GoProgram ()) -> GoChan -> GoChan -> GoProgram GoChan
chan_pipe action inchan@(inchan_put, _) outchan@(_, outchan_get) = do
  go $ action inchan outchan
  return (inchan_put, outchan_get)

-- like chan_pipe, but the output channel is created
chan_transform :: (GoChan -> GoChan -> GoProgram ()) -> GoChan -> GoProgram GoChan
chan_transform action chan = newchan >>= chan_pipe action chan


chan_filter :: (GoExpr -> Bool) -> GoChan -> GoProgram GoChan
chan_filter pred = do
  chan_transform filter_goroutine 
  where filter_goroutine inchan outchan = do
          val <- getchan inchan
          if pred val
          then do
            putchan outchan val
            filter_goroutine inchan outchan
          else do
            filter_goroutine inchan outchan


chan_map :: GoExprable a => (GoExpr -> a) -> GoChan -> GoProgram GoChan
chan_map f = do
  chan_transform map_goroutine
  where map_goroutine inchan outchan = do
          val <- getchan inchan
          putchan outchan $ toGoExpr $ f val
          map_goroutine inchan outchan


chan_fold :: GoExprable a => a -> (GoExpr -> a -> a) -> GoChan -> GoProgram GoChan
chan_fold init f = do
  chan_transform (fold_goroutine init)
  where fold_goroutine acc inchan outchan = do 
          val <- getchan inchan
          let acc' = f val acc
          putchan outchan $ toGoExpr acc'
          fold_goroutine acc' inchan outchan

-- partition an input channel into two output channels
chan_partition :: (GoExpr -> Bool) -> GoChan -> GoProgram (GoChan, GoChan)
chan_partition pred chan@(chan_put, _) = do
  lchan@(_, lchan_get) <- newchan
  rchan@(_, rchan_get) <- newchan
  let lchan' = (chan_put, rchan_get)
  let rchan' = (chan_put, lchan_get)
  go $ partition_goroutine lchan rchan
  return (lchan', rchan')
  where partition_goroutine lchan rchan = do
          val <- getchan chan
          if pred val
          then do 
            putchan lchan val
            partition_goroutine lchan rchan
          else do
            putchan rchan val
            partition_goroutine lchan rchan


{-
 - how to do this properly?
chan_merge :: GoChan -> GoChan -> GoProgram GoChan
chan_merge chan1 chan2 = do
  outchan <- newchan
  go $ merge_goroutine
  where merge_

-}


-- channel creation functions

-- given a function to produce channel values, create a new channel
chan_produce :: (GoChan -> GoProgram ()) -> GoProgram GoChan
chan_produce action = do
  chan <- newchan
  go $ action chan
  return chan


chan_repeat :: GoExprable a => a -> GoProgram GoChan
chan_repeat x = do
  chan_produce repeat_goroutine
  where repeat_goroutine chan = do
          putchan chan x
          repeat_goroutine chan


chan_iterate :: GoExprable a => a -> (a -> a) -> GoProgram GoChan
chan_iterate x f = do
  chan_produce $ iterate_goroutine x
  where iterate_goroutine acc chan = do
          putchan chan (toGoExpr acc)
          iterate_goroutine (f acc) chan


chan_cycle :: GoExprable a => [a] -> GoProgram GoChan
chan_cycle lst = do
  chan_produce $ cycle_goroutine lst
  where cycle_goroutine [] chan = cycle_goroutine lst chan
        cycle_goroutine (x:xs) chan = do
          putchan chan (toGoExpr x)
          cycle_goroutine xs chan 


chan_unfoldr :: GoExprable a => b -> (b -> (a, b)) -> GoProgram GoChan
chan_unfoldr x f = do
  chan_produce $ unfoldr_goroutine x
  where unfoldr_goroutine acc chan = do
          let (v, acc') = f acc
          putchan chan $ toGoExpr v
          unfoldr_goroutine acc' chan



-- misc channel functions

-- get multiple items from the channel at once
-- this blocks until all items are received
chan_take :: Int -> GoChan -> GoProgram [GoExpr]
chan_take 0 chan = return []
chan_take n chan = do
  hd <- getchan chan
  tl <- chan_take (n-1) chan
  return $ hd:tl


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
          

---

evenmain = do
  countchan <- chan_iterate (0 :: Int) (+1)
  (evenchan, oddchan) <- chan_partition (\(GoInt x) -> x `mod` 2 == 0) countchan
  go $ chan_take 100 oddchan >>= goprint . show
  go $ chan_take 100 evenchan >>= goprint . show
  done

