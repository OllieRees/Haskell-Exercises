-- define a function using foldl that turns a list of base 10 digits to it's integer equiv.
dec2Int :: [Int] -> Int
dec2Int (digits) = foldl (\xs -> \x -> xs + (10 * x)) 0 (reverse digits)

