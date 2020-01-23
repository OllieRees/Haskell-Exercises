-- redefine last

last_1' xs = head (reverse xs)

last_2' xs = xs !! ((length xs) - 1)