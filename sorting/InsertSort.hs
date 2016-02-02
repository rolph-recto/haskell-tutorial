insertSort xs = foldr insert [] xs
  where insert y []     = [y]
        insert y (x:xs) = if y > x then x:(insert y xs) else y:x:xs
