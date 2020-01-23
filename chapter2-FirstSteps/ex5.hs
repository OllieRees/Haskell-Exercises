--redefine init

init_1' xs = reverse (tail (reverse xs))

init_2' xs = [a | a <- xs, a /= last xs]

-- Recursive function which takes the head of each list adding them on until it reaches a singleton
