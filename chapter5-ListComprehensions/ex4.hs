-- Redefine the library function replicate
replicate' :: Int -> a -> [a]
replicate' n a = [a | i <- [1..n]]