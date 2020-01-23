-- Formulate the following pattern matching with conditionals
andPatMat :: Bool -> Bool -> Bool
andPatMat True b = b
andPatMat False _ = False


andCond :: Bool -> Bool -> Bool
andCond x y = if x == True then y else False

-- No nested if statement meaning two less conditionals (the if and else within the first if statement).