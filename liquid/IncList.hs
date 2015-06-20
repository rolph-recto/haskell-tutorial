module IncList () where

{-@ data IncList a = Emp | (:<) { hd :: a, tl :: IncList {v:a | hd <= v} } @-}
data IncList a = Emp
              | (:<) { hd :: a, tl :: IncList a }

infixr 9 :<

okList = 1 :< 2 :< 3 :< Emp

-- badList = 2 :< 1 :< 3 :< Emp

insertSort :: (Ord a) => [a] -> IncList a
insertSort [] = Emp
insertSort (x:xs) = insert x (insertSort xs)

insert :: (Ord a) => a -> IncList a -> IncList a
insert y Emp = y :< Emp
insert y (x :< xs)
  | y <= x    = y :< x :< xs
  | otherwise = x :< insert y xs
