-- Define sum
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Define take
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:(take' (n - 1) xs)

-- Define last
last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs  