-- Get third element using head AND tail
third_one :: [a] -> a
third_one xs = head (tail (tail xs))

-- Get third element using !!
third_two :: [a] -> a
third_two xs = xs !! 2

-- Get third element using pattern matching
third_three :: [a] -> a
third_three (_:_:x:xs) = x 