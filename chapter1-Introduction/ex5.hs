--replace <= with < in the qsort function and justify what happens

qsort :: Ord n => [n] -> [n]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger where
    smaller = [s | s <- xs, s < x]
    larger = [l | l <- xs, l > x]

-- What should happen is that if x is repeated in the list then it won't be included in smaller and larger, and so will go away. 
