-- Show how length [1, 2, 3] is evaluated
length' :: [Int] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- length' (1:[2, 3]) = 1 + length' [2, 3]
-- length' (2:[3]) = 1 + length' [3]
-- length' (3:[]) = 1 + length' []
-- length' [] = 0
-- length' (1:[2, 3]) = 1 + length' [2, 3] = 1 + (1 + length' [3]) = 1 + (1 + (1 + length'[])) 
-- = 1 + (1 + (1 + 0)) = 1 + (1 + 1) = 1 + 2 = 3

-- Show how drop 3 [1, 2, 3, 4, 5] is evaluated
drop' :: Int -> [Int] -> [Int]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop (n - 1) (xs)

-- drop 3 1:[2, 3, 4, 5] = drop 2 [2, 3, 4, 5]
-- drop 2 2:[3, 4, 5] = drop 1 [3, 4, 5]
-- drop 1 3:[4, 5] = drop 0 [4, 5]
-- drop 0 [4, 5] = [4, 5]
-- drop 3 1:[2, 3, 4, 5] = drop 2 [2, 3, 4, 5] = drop 1 [3, 4, 5] = drop 0 [4, 5] = [4, 5]

-- Show how init [1, 2, 3] is evaluated
init' :: [Int] -> [Int]
init' [x] = []
init' (x:xs) = x:(init xs)

-- init 1:[2, 3] = 1:(init [2, 3])
-- init 2:[3] = 2:(init [3])
-- init 3:[] = []
-- init 1:[2, 3] = 1:(init [2, 3]) = 1:(2:init[3]) = 1:(2:[]) = [1, 2]