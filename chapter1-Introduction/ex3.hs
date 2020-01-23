-- implement a product function that outputs the product of every element in a list.
-- prove that the function works by using the example list [2, 3, 4]

module Main where

product' :: Num a => [a] -> a
product' [x] = x --what about if it's literally an empty list 
product' (x : xs) = x * product'(xs)

main = do 
        print(product' [2, 3, 4])
