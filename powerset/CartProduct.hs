module CartProduct (
  cartProduct
) where

cartProduct :: [[a]] -> [[a]]
cartProduct []      = [[]]
cartProduct (x:xs)  = x >>= (\e -> map (e:) (cartProduct xs))
