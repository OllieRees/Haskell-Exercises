-- Redefine a function which gets the position of an element x from the list xs using the find function
find' :: Eq a => a -> [(a, b)] -> [b]
find' key map = [v | (k, v) <- map, k == key]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find' x mapping 
    where
        mapping = ([(x', i) | (x', i) <- (zip xs [0..]) ]) -- make a map of (k, i) where k is every element in xs