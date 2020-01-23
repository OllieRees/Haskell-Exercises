-- Some examples of functions with conditionals : [if (boolean expr) then (returning val) else] ... 
greatest :: Ord a => (a, a) -> a
greatest (x, y) = if x > y then x else y

isUppercase :: Char -> Bool
isUppercase c = if c >= 'A' && c <= 'Z' then True else False

-- Guards are a more readable way of using if statements (preferable to use them unless the condidtionals are nested (see ex5))

greatest' :: Ord a => (a, a) -> a
greatest' (x, y)
    | x > y         = x
    | otherwise     = y

isUppercase' :: Char -> Bool
isUppercase' c 
    | c >= 'A' && c <= 'Z'  = True
    | otherwise             = False