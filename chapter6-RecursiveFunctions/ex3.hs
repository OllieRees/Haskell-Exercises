-- Define an exponent function recursively, and prove it works with 2^3
module Main where

    pow :: Int -> Int -> Int
    pow 0 _ = 0
    pow _ 0 = 1
    pow base exp = base * (pow base (exp - 1))
    
    main = do
        print (pow 2 3)