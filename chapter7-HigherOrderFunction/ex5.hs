-- Define curry 
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y  = f (x, y)

-- Define uncurry
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y