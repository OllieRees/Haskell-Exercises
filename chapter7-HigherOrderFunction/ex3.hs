-- Redefine map and filter using foldr

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x -> \xs -> (f x) : xs) [] xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x -> \xs -> if p x == True then x : xs else xs) [] xs -- x : xs if p x == True; else xs.