find' :: Eq a => a -> [(a, b)] -> [b]
find' key map = [v | (k, v) <- map, k == key] 

-- Zip is a function that combines each element i of two lists into a tuple.
pairs' :: [a] -> [(a, a)]
pairs' xs = zip xs (tail xs)

sorted' :: Ord a => [a] -> Bool
sorted' xs = and [x <= y | (x, y) <- pairs' xs]

-- Checks if a sequence is arithmetic
checkSequence :: [Int] -> Int -> Bool
checkSequence xs r = and [(x + r) == y | (x, y) <- (pairs' xs)]

checkSequence' :: [Int] -> Int -> Bool
checkSequence' xs r = if [(x, y) | (x, y) <- (pairs' xs), (x + r) == y] == (pairs' xs) then True else False

-- Proof that zips makes a function more concise.
positionsWOZips :: Ord a => [a] -> a -> [Int]
positionsWOZips xs x = [a | a <- [0 .. ((length xs) - 1)], x == (xs !! a)]

positionsWZips :: Ord a => [a] -> a -> [Int]
positionsWZips xs x = [i | (i, v) <- zip [0 ..] xs, v == x]