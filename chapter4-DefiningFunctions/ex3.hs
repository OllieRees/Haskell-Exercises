-- Safetail is the same as tail but if the list is empty it returns the empty list

-- Define safetail using conditionals
safetail_one :: [a] -> [a]
safetail_one xs = if null xs then [] else tail xs

-- Define safetail using guards
safetail_two :: [a] -> [a]
safetail_two xs 
    | null xs   = []
    | otherwise = tail xs

-- Define safetail using pattern matching
safetail_three :: [a] -> [a]
safetail_three [] = []
safetail_three xs = tail xs -- We could also do safetail_three (_:xs) = xs