-- Update the qsort method to reverse the sorted list

r_qsort :: Ord n => [n] -> [n]

r_qsort [x] = [x] -- base case : singleton list is already sorted
r_qsort (x:xs) = r_qsort larger ++ [x] ++ r_qsort smaller where
    larger = [l | l <- xs, l > x]
    smaller = [s | s <- xs, s <= x]

-- Another (significantly worse) way
qsort :: Ord n => [n] -> [n]
qsort [] = [] -- base case - we can also use an empty list (more recursive calls though)
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
            where
                smaller = [a | a <- xs, a  <= x]
                larger = [b | b <- xs, b > x]

r_qsort' :: Ord n => [n] -> [n]
r_qsort' x = reverse (qsort x) 