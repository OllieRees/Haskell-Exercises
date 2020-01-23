-- Halve a List of even length such that xs becomes (x, y) where x ++ y == xs
halve :: [a] -> ([a], [a])
halve xs
    | even (length xs)   = ((take (div (length xs) 2) xs), (drop (div (length xs) 2) xs))
    | otherwise                 = (init xs, tail xs) -- I don't know what do here 