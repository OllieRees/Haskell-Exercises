-- Do NOT run these functions; they're just examples of how lambda expressions work. You'd use these as a one-off, not as a function. 

-- Lambda expressions are nameless functions, with multiple purposes in programming. 

-- Lambda expressions can simplify curried functions.

multiply :: Num a => a -> (a -> a) -- Takes a function, which in turn takes a function which outputs a.
multiply = \x -> (\y -> x * y)  
-- multiply can be represented as f(x) -> g(y) where g(y) -> x * y, as such f: x -> (y -> (y * x)) 

add' :: Num a => a -> (a -> a) 
add' = \x -> (\y -> x + y)
-- add' is represented as f(x) -> g(y) where g(y) -> x + y as such f: x -> (y -> (x + y))

createList :: a -> (a -> [a]) 
createList = \x -> (\y -> x:y:[])
-- createList is represented as f(x) -> g(y) where g(y) -> x:y:[] as such f: x -> (y -> (x:y:[]))

scalar' :: Num a => a -> (a -> (a -> [a]))
scalar' x y = \lambda -> (\x -> (\y -> (lambda * x):(lambda * y):[]))
-- scalar' is represnted as f(lambda) -> g(x) where g(x) -> h(y) where h(y) -> (lambda * x):(lambda * y):[].
-- As such f: lambda -> (x -> (y -> ((lambda * x):(lambda * y):[]))) 

-- Lambda expressions are also useful for defining functions that return functions instead of values
const' :: a -> (b -> a)
const' x = \_ -> x

-- Lambda expressions can be used as a one-off function instead of defining a function in the where clause
odds' :: Int -> [Int]
odds' n = map (\x -> x * 2 + 1) [0..n-1]

multiplesOfEleven :: Int -> [Int]
multiplesOfEleven n = map (\x -> 11 * x) [0..n-1]

succList :: Int -> [Int]
succList n = map (\x -> x + 1 ) [0..n-1]