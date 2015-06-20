module Exercise4_2 (test2) where


{-@ type Nat = {v:Int | 0 <= v} @-}
{-@ type Btwn Lo Hi = {v:Int | Lo <= v && v <= Hi} @-}

{-@ data Sparse a = SP { spDim :: Nat, spElems :: [(Btwn 0 spDim, a)] } @-}
data Sparse a = SP { spDim :: Int, spElems :: [(Int, a)] }

{-@ type SparseN a N = {v:Sparse a | spDim v == N} @-}

{-
Exercise 4.2. [Addition] Write the specification and implementation
of a function plus that performs the addition of two Sparse vectors of
the same dimension, yielding an output of that dimension. When you
are done, the following code should typecheck:
-}

{-@ plus :: x:Sparse a -> {y:Sparse a | spDim x == spDim y} -> {z:Sparse a | spDim z = spDim x} @-}
plus :: (Num a) => Sparse a -> Sparse a -> Sparse a
plus x y = x

{-@ test2 :: SparseN Int 3 @-}
test2 :: Sparse Int
test2 = plus vec1 vec2
  where
    vec1 = SP 3 [(0, 12), (2, 9)]
    vec2 = SP 3 [(0, 8), (1, 100)]
