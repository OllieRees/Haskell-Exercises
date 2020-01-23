-- How does fact behave to a negative argument:

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- If n < 0, then it will cause the recursive clause which will compute fact (n - 1), where n - 1 < 0. This will result in no base case being executed leading to infinite recursion.

factWithGuard :: Int -> Int
factWithGuard 0 = 1 
factWithGuard n 
    | n > 0     = n * fact (n - 1)
    | otherwise = 0
-- Could probably have another pattern match for when n is less than 0 instead of adding it as a guard to the recursive clause
