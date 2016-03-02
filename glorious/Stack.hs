import Control.Monad (forM_)

push :: a -> [a] -> ([a], ())
push x s = (x:s, ())

pop :: [a] -> ([a], Maybe a)
pop []	    = ([], Nothing)
pop (x:xs)  = (xs, Just x)

stackStateless :: IO ()
stackStateless = do
  let s = []
  let (s2, _) = push 1 s
  let (s3, _) = push 2 s2
  let (s4, Just x4) = pop s3
  let (s5, _) = push (x4*2) s4
  let (_, x6) = pop s5
  print x6

-- State monad
newtype State s a = State (s -> (s,a))

instance Monad (State s) where
  -- return :: a -> m a 
  return x = State (\s -> (s,x))

  -- (>>=) :: m a -> (a -> m b) -> m b
  (State f) >>= g = State $ \s -> 
    let (s', x) = f s in
    let (State new) = g x in
    new s'

  -- (>>) :: m a -> m b -> m b
  f >> g = f >>= \_ -> g

get :: State s s
get = State (\x -> (x,x))

put :: s -> State s ()
put x' = State (\x -> (x',()))

runState :: s -> State s a -> (s,a)
runState x (State f) = f x

execState :: s -> State s a -> s
execState x = fst . runState x

evalState :: s -> State s a -> a
evalState x = snd . runState x

pushState :: a -> State [a] ()
pushState x = do
  stack <- get
  put (x:stack)

popState :: State [a] (Maybe a)
popState = do
  stack <- get
  case stack of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return (Just x)

-- equivalent to stackStateless
stackAction :: State [Int] (Maybe Int)
stackAction = do
  pushState 1
  pushState 2
  Just x <- popState
  pushState (x*2)
  popState

-- this is equivalent to the above
stackAction' = pushState 1 >> pushState 2 >> popState >>= (\(Just x) -> pushState (x*2) >> popState)

stackStateful :: IO ()
stackStateful = print $ evalState [] stackAction

-- push a range of values to the stack
stackAction2 :: Int -> State [Int] ()
stackAction2 x = do
  forM_ [1..x] pushState

stackStateful2 :: IO ()
stackStateful2 = print $ execState [] (stackAction2 10)

{-
How would you write the above in OCaml?

let push x stack =
  stack := x :: !stack
;;

let pop stack =
  match !stack with
  | []    -> None
  | x::xs -> begin
    stack := xs;
    Some x
  end
;;

let stackAction =
  let stack = ref [] in
  push 1 stack;
  push 2 stack;
  let Some x = pop stack in
  push (x*2) stack;
  let Some x' = pop stack in
  print_int x'
;;

-}

main = do
  stackStateful
  stackStateless
  stackStateful2 

