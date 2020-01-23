-- Define mergesort
halve :: [a] -> ([a], [a])
halve xs = ( take half xs , drop (length xs - half) xs ) 
    where 
        half = (length xs) `div` 2
        
merge :: Ord a => ([a], [a]) -> [a]
merge ([], b) = b
merge (a, []) = a
merge ( a1@(x:xs), a2@(y:ys) )
    | x <= y     = x:( merge (xs, a2) )
    | otherwise  = y:( merge (a1, ys) )

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort a, msort b)
        where
            a = fst (halve xs)
            b = snd (halve xs)
