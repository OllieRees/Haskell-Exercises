-- Define std. functions using higher order functions

-- Decide if all the elements in a list hold for a predicated
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = length (filter p xs) == length xs

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = length ( filter p xs ) > 0

-- These examples don't really use higher order functions like fold, iterate, map or filter.

-- Take the elements of xs while p holds
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = take (head ( [ i | (x, i) <- zip xs [0..], p x == False ] ) ) xs

-- Take the elements of xs after p fails
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) -- drop the list as soon as it's false
    | p x == False      = x:xs
    | otherwise         = dropWhile' p xs