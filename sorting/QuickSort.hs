quickSort []      = []
quickSort [x]     = [x]
quickSort (x:xs)  = quickSort left ++ [x] ++ quickSort right
  where (left, right) = partition x xs
        partition pivot = foldr (part pivot) ([], [])
        part x item (l, r) = if x > item then (item:l, r) else (l, item:r)
