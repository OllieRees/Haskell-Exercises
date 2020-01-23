lsComprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lsComprehension f p xs = [f x | x <- xs, p x]

ls' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
ls' f p xs = map f (filter p xs)