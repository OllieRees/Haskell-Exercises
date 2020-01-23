-- Pattern Matching  : a to specify what outputs certain inputs produce. 
booleanAnd :: Bool -> Bool -> Bool
booleanAnd True True    = True
booleanAnd _ _          = False -- _ is a wildcard pattern (can represent any input provided it's an element of the input set)

-- Another way to define bitwise and is by checking if it has a True input and returning the value of the other input
booleanOr :: Bool -> Bool -> Bool
booleanOr False b   = b
booleanOr True _    = True

-- Using guards to do the job (Not always convertible)
booleanXOR :: Bool -> Bool -> Bool
booleanXOR bOne bTwo
    | bOne == bTwo = False
    | bOne /= bTwo = True 

-- Pattern Matching can be done on Tuples and Lists in a similar way
getFirst :: (a, b) -> a
getFirst (x, _) = x

startsOne :: [Int] -> Bool
startsOne (1:_)    = True -- Use con so that list pattern can be of varying length
startsOne _         = False

isSingleton :: [Int] -> Bool
isSingleton [_]  = True
isSingleton _       = False

-- Using the con operator
tail' :: [a] -> [a]
tail' (_:xs) = xs 

appendToHead :: a -> [a] -> [a]
appendToHead fe xs = fe:xs

appendToTail :: [a] -> a -> [a]
appendToTail xs le = reverse (le:(reverse xs))