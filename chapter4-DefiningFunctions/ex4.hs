-- Define OR (||) in 4 different ways (using function names and not operations for testing purposes)

-- Determine the only case when || returns false
or_one :: Bool -> Bool -> Bool
or_one False False = False
or_one _ _ = True

-- Utilise the pattern that says when one arg. is True, then the whole thing is true.
or_two  :: Bool -> Bool -> Bool
or_two True _  = False
or_two False b = b

-- Split the table up based on if the inputs have the same value (have to use guards, unfortunately)
or_three :: Bool -> Bool -> Bool
or_three b c 
    | b /= c                    = True
    | b == True || c == True    = True
    | otherwise                 = False


-- The really long way
or_four :: Bool -> Bool -> Bool
or_four True True = True
or_four True False = True
or_four False True = True
or_four False False = False