--Define bitwise and
and' :: [Bool] -> Bool
and' [True] = True
and' (b:bs) 
    | b == False    = False
    | otherwise     = and' bs

-- Define concat
concat' :: [[a]] -> [a]
concat' [] = []
concat' [xs] = xs
concat' (x:xs) = x ++ concat' xs

-- Define replicate
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:(replicate (n - 1) x)

-- Define getElem
getElem :: [a] -> Int -> a
getElem xs 0 = head xs
getElem (_:xs) n = getElem xs (n - 1) 

-- Define elem
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x' (x:xs) 
    | x == x'   = True
    | otherwise = elem' x' xs