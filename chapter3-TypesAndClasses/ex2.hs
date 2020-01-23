bools :: [Bool]
bools = [True, False, False, True]

nums :: [[Int]]
nums = [[1..x] | x <- [1..10]] 

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

-- Takes a single-arg function and the parameter for that function, i.e. apply reverse [1..10]
apply :: (a -> b) -> a -> b
apply func_arg a = func_arg a 