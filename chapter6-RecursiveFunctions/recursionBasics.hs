-- Recursion is a method of looping in functional languages
fac :: Int -> Int
fac 0 = 1               -- base case
fac n = n * fac (n - 1) -- recursive clause

triNum :: Int -> Int
triNum 0 = 0
triNum n = n + triNum (n - 1)

-- (*) is an example of a recursively-defined operator
multiply :: Int -> Int -> Int
multiply x 0 = 0
multiply x y = x + (multiply x (y - 1))

-- Recursion can be done on lists too
length' :: [Int] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x] 
insert' x (y:ys) 
    | x <= y        = x:ys
    | otherwise     = y:(insert' x ys)

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insortionSort (x:xs) = insert' x (insertionSort xs)

-- Recursion can take multiple arguments
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = [] 
drop' n (x:xs) = (drop (n - 1) xs)

-- The recursive clause can call itself multiple times, as well.
fib' :: Int -> Int
fib' 0 = 1
fib' 1 = 1
fib' n = fib'(n - 1) + fib'(n - 2) -- Calls Fub' twice.

-- The recursive clause can contain other functions that are recursively defined. When this happens the functions are mutually recursive.
even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

