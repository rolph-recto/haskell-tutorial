
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM f [] = return []
filterM f (x:xs) = do
  b <- f x
  if b then do
    tl <- filterM f xs
    return (x:tl)
  else filterM f xs
