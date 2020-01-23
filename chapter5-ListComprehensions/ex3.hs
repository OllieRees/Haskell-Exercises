-- Define a method that returns a list of the possible coordinates of a n-by-n checkers grid 
square :: Int -> [(Int, Int)]
square n = [(x, y) | x <- [0..n], y <- [0..n], x /= y]