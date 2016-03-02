
data Option a = Some a | None deriving (Show)

instance Monad Option where
  return x = Some x

  None >>= f	= None
  Some x >>= f	= f x

-- we go out of balance if |x| > 2

tiltRight :: Int -> Option Int
tiltRight x
  | x >= 2    = None
  | otherwise = Some (x + 1)

tiltLeft :: Int -> Option Int
tiltLeft x
  | x <= -2   = None
  | otherwise = Some (x - 1)

balance :: Int -> Option Int
balance n = do
  x1 <- tiltRight n
  x2 <- tiltRight x1
  x3 <- tiltLeft x2
  x4 <- tiltRight x3
  x5 <- tiltRight x4
  x6 <- tiltRight x5
  x7 <- tiltLeft x5
  return x7

balance2 :: Int -> Option Int
balance2 n = do
  x1 <- tiltLeft n
  x2 <- tiltLeft x1
  x3 <- tiltRight x2
  x4 <- tiltRight x3
  x5 <- tiltLeft x4
  return x5

main = do
  print $ balance 0 -- we f
  print $ balance2 0
