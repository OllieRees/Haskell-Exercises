-- Define a function for producing the scalar product of two lists, which is the sum of the products of each element of same index.
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs1 xs2 = sum [ (x * y) | (x, y) <- (zip xs1 xs2) ] 