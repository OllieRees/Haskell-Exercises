-- Formulate the following pattern matching with conditionals

andPatMat :: Bool -> Bool -> Bool
andPatMat True True = True
andPatMat _ _ = False

andCond :: Bool -> Bool -> Bool
andCond x y = if x == True then if y == True then True else False else False
