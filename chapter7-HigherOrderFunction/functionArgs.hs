import Data.Char

-- Functions can take other functions as arguments.
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- map takes a function that takes a and outputs b as well as the INPUTS of the function; it outputs the values of the function. 
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- Filter creates a subset of the given list where the elements of the resulting list held for the given predicate function
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x] 

-- The predicate function can be an OPERATION which takes an element from the list and compares it to a given value
nonZeroes :: [Int] -> [Int]
nonZeroes ns = filter' (== 0) ns

-- The predicate function can be a FUNCTION which takes an element from the list and uses it as input for a function which returns a Boolean value.
isZero :: Int -> Bool
isZero n = n == 0

nonZeroes' :: [Int] -> [Int]
nonZeroes' ns = filter' isZero ns

-- Functions often use filter and map together to apply a function to certain elements from a list.
charToInt :: Char -> Int
charToInt c = ord c - ord '0'

string2Ints :: String -> [Int]
string2Ints str = map charToInt (filter isDigit str) 

doubleEvens :: String -> [Int]
doubleEvens str = map (*2) ( filter even (string2Ints str) )

-- Some implementations of prelude functions
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = length (filter' p xs) == length xs   

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = length (filter' p xs) > 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = take ( head ([i | (x, i) <- zip xs [0 ..], p x == False]) ) xs -- In practice we don't need to create a list of the elements that don't hold the predicate

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p xs = drop ( head ([i | (x, i) <- zip xs [0..], p x == False]) ) xs 

-- We can use function composition to have a pipe the output of functions as the input of another function.
odd' :: Int -> Bool
odd' n = (not . even ) (n)

string2Ints' :: String -> [Int]
string2Ints' str = ( map charToInt . filter (isDigit) ) (str)