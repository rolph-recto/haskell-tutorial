module Powerset (
  powerset
) where

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = let ptail = powerset xs in addHead x ptail ++ ptail
  where addHead elem tail = map (\list-> elem:list) tail

cartProduct :: [[a]] -> [[a]]
cartProduct []      = [[]]
cartProduct (x:xs)  = x >>= (\e -> map (e:) (cartProduct xs))

data EraseItem = EraseLHS { subc :: Int }
               | EraseRHS { subc :: Int }
               deriving (Show)

