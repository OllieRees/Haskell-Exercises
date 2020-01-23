altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f (xs)

luhnDouble :: Int -> Int
luhnDouble x 
    | 2 * x > 9     = 2 * x - 9
    | otherwise     = 2 * x

luhn :: [Int] -> Bool
luhn xs = (sum (altMap luhnDouble (\x -> x) xs)) `mod` 10 == 0