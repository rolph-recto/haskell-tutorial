mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort xx) (mergeSort yy)
  where (xx, yy) = splitAt (length xs `div` 2) xs
        merge [] ys = ys
        merge xs [] = xs
        merge xx@(x:xs) yy@(y:ys) =
          if x < y then x:(merge xs yy) else y:(merge xx ys)
