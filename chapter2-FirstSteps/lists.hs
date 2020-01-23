-- This script uses example functions to illustrate how basic list functions work

-- Uses indexing
get_OR :: Num a => [a] -> Int -> a -- takes two arguments : [Num] and Integer. Produces a Num result
get_OR ls i = ls !! i

-- Uses head, last, init and tail (linked list (really an array))
insertHead :: Num a => [a] -> a -> a
insertTail :: Num a => [a] -> a -> a
removeHead :: Num a => [a] -> [a]
removeTail :: Num a => [a] -> [a]
insertHead x y = head (y:x)
insertTail x y = last(x ++ [y])
removeHead x = tail(x) 
removeTail x = init(x) 

-- Uses drop and take (add function declarations (need to learn this))
halfList x = div (length x) 2
splitList x = (drop (halfList x) x) ++ (take (halfList x) x)

-- Uses length and null
last' :: Num a => [a] -> a
last' x 
    | null x /= True = x !! (length x - 1) -- make sure null x is False
    | null x == True = 0

-- Uses product and sum
triangularNum :: Integer -> Integer
triangularNum n = sum [1..n]

factorial :: Integer -> Integer
factorial n = product [1..n]

-- Uses appending (++ and :)
addHead :: Num a => [a] -> [a]
addHead x = x ++ [y]
    where
        y = head x

addTail :: Num a => [a] -> [a]
addTail x = y:x
    where
        y = last x

-- Uses reverse
new_head :: Num a => [a] -> a
new_head x = last (reverse x)