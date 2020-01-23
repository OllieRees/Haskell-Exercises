-- Haskell allows lists to be formed in the format [f(x) | x e SET . predicateRules]

squares :: Int -> [Int]
squares n = [x^2 | x <- [1..n]] 

-- A list can have more than one generator; in this case the result is the cartesian product of both lists.
studentPass :: [String] -> [(String, Bool)]
studentPass students = [(s, b) | s <- students, b <- [True, False]]

-- Changing the order of the generators is declared changes the ordering of the elements.
-- This is because the list comprehension loops around the last list first : the last list is the inner-most nest.
studentPass' :: [String] -> [(String, Bool)]
studentPass' students = [(s, b) | b <- [True, False], s <- students] 

-- A practical example : concatanation of a 2D list
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- Wildcards can be used within list comprehension too
length' :: [a] -> Int
length' xs = sum [1 | _ <- xs] -- The element being iterated isn't used in the equal equation, so it's not needed.

-- Guards / conditional expressions can be used within list comprehensions too!
find' :: Eq a => a -> [(a, b)] -> [b]
find' key map = [v | (k, v) <- map, k == key] 

factors' :: Int -> [Int]
factors' n = [x | x <- [1..n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n
    | (length' (factors' n)) == 2   = True
    | otherwise                     = False

primes' :: Int -> [Int]
primes' n = [x | x <- [2..n], isPrime x]

nthPrime :: Int -> Int -> Int
nthPrime n i = if length (primes' i) == n then last (primes' i) else nthPrime n (i+1) -- really need to remove the arg. i