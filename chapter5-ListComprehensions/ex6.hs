-- Return all perfect numbers up to a limit where a perfect number is a number (x) which is equal to the sum of its factors
factors' :: Int -> [Int]
factors' n = [f | f <- [1..n], (mod n f) == 0]

perfects :: Int -> [Int]
perfects n = [p | p <- [1..n], p == sum (init (factors' p))]