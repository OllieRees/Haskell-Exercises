-- Define euclid's algorithm using recursion
euclid :: Int -> Int -> Int
euclid a b 
    | a == b    = a                 -- Base case
    | a > b     = euclid b (a - b)
    | otherwise = euclid a (b - a)

-- I don't know if this method is legitimate, but it works where euclid a b == euclid b a

