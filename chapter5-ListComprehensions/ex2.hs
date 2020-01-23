-- Define a method that returns all the possible coordinates of a m-by-n grid
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]