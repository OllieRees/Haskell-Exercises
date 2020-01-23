-- Find the sum of the first 100 squares
module Main where
    main = do 
        print (sum [x^2 | x <- [1..100]])