-- Define a merge function which merges two sorted lists to form a bigger sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (x:xs) (y:ys)
    | x <= y     = x:(merge xs (y:ys))
    | otherwise  = y:(merge (x:xs) ys) 

-- Where could I use insertion or insertion sort? 