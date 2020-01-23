-- Foldr is a high-order function which generalises recursive defintions which take the form f (x:xs) = x # f xs where # is an operation.

-- Such a function, for example, is sum where the operator is (+)
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

summationLists :: Num a => [a] -> a
summationLists xs = foldr (+) 0 xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || (or' xs) 
 
orFoldr :: [Bool] -> Bool
orFoldr xs = foldr (||) False xs

-- This function is definable similar to recursive functions that use it
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f baseRes [] = baseRes
foldr' f baseRes (x:xs) = f x (foldr' f baseRes xs)

-- Foldr doesn't always need to take an operation between elements, too.
lengthFoldr :: [a] -> Int
lengthFoldr xs = foldr (\_ n -> n + 1) 0 xs -- The lambda func. takes 2 params with the second param being incremented.

-- Foldr can also deal with changing the structure with the list instead of dealing with it's components. 
reverseFoldr :: [a] -> [a]
reverseFoldr xs = foldr (\h -> \t -> t ++ [h]) [] xs

-- Similarly, foldl does the same but for functions which are represented as f xs # x, where # is (again) an operation
foldl' :: (b -> a -> b) -> b -> [a] -> b 
foldl' f t [] = t
foldl' f t (x:xs) = f (foldl' f t xs) x

lengthFoldl :: [a] -> Int
lengthFoldl xs = foldl ( \n _ -> n + 1 ) 0 xs

reverseFoldl :: [a] -> [a]
reverseFoldl xs = foldl ( \t -> (\h -> t ++ [h]) ) [] xs