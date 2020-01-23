-- Variables using STD types
f :: Bool
f = False

a :: Int
a = (2^63 - 1)

b :: Integer
b = 2^63 + 1

pi_f :: Float
pi_f = 3.14

pi_d :: Double
pi_d = 22/7

-- Function using STD type Bool
not' :: Bool -> Bool
not' b 
    | b == True = False
    | b == False = True 

even' :: Int -> Bool
even' n 
    | mod n 2 == 0 = True
    | mod n 2 == 1 = False

-- Variables using lists
firstTwenty :: [Int]
firstTwenty = [1..20]

twoDecimalPlaces :: [Float]
twoDecimalPlaces = [0.01, 0.02, 1]

uppercaseChars :: [Char]
uppercaseChars = ['A'..'Z']

-- Variables using 2-d lists
emptyList :: Eq a => [a]
emptyList = []

singletonList :: Eq a => [[a]]
singletonList = [[]]

-- Function using Lists ()
-- Improve this method
longestList :: [[Int]] -> [Int]
longestList iss = longestList
    where
        lengths :: [Int]
        longestLength :: Int
        longestList :: [Int]
        lengths = [length ll | ll <- iss]
        longestLength = maximum lengths
        longestList = head [ls | ls <- iss, length ls == longestLength] -- returns the first one

-- Tuples 
hashmap' :: (String, Int)
hashmap' = ("UK Dialing Code", 44)

asciiVal :: ([Char], [Int], [Char], [Int])
asciiVal = (['A'..'Z'], [65..90], ['a'..'z'], [97..122])

booleanInts :: ((Bool, Int), (Bool, Int))
booleanInts = ((False, 0), (True, 1))

-- Using multiple parameters : use tuples or lists
add' :: (Int, Int) -> Int
add' (x, y) = x + y

subtract' :: (Int, Int) -> Int 
subtract' (x, y) = x - y

predSucc :: Int -> (Int, Int)
predSucc x = (x - 1, x + 1) 

addAllNumbers :: [Int] -> Int -- Same as sum [Int]
addAllNumbers [] = 0
addAllNumbers (n:ns) = n + addAllNumbers(ns)

-- Curried Functions (really breaking into functional programming here)
mult :: Int -> Int -> Int -> Int
mult x y z = x * (y * (z))

sameParity :: Int -> Int -> Bool
sameParity a b 
        | (mod a 2) == (mod b 2) = True
        | (mod a 2) /= (mod b 2) = False

splitAtN :: [a] -> Int -> ([a], [a])
splitAtN xs i = (take i xs, drop i xs)

-- Can we think of it as a simple conversion from tuples to currying (curry and uncurry functions)?

-- Polymorphic functions
append' :: a -> a -> [a]
append' x y = [x] ++ [y]

-- Using Classes
-- Equality types : uses == or /=
isEqual :: Eq a => a -> a -> Bool
isEqual a b 
        | a == b        = True
        | otherwise     = False

-- Ordered types : superset of EQ - uses comparison operators
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger 
        where 
            smaller = [a | a <- xs, a <= x]
            larger = [b | b <- xs, b > x]

-- Showable types : Convertable to String
show' :: (Show a) => a -> String
show' x = show x

-- Readable types : Can be converted from String
read' :: (Read a) => String -> a
read' x = read x

-- Numeric types : Numbers
succ' :: Num a => a -> a
succ' x = x + 1

-- Integral types : Subset of Num - can be divided
multipleOfSeven :: (Integral a) => a -> Bool
multipleOfSeven x 
            | mod x 7 == 0  = True
            | otherwise     = False

-- Fractional types : Subset of Num - fractions
recip :: (Fractional a) => a -> a
recip n = 1/n