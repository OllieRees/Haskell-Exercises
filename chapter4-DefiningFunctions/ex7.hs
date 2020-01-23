-- Check if a 4 digit card number is valid using the luhn algorithm.
luhnDouble :: Int -> Int
luhnDouble x 
    | 2 * x > 9     = 2 * x - 9
    | otherwise     = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (b + d + (luhnDouble a + luhnDouble c)) 10 == 0 