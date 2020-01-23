-- Prove that for a singleton list x, that sum x = x : sum [x] = x
-- for a singleton list sl => sum sl -> sl

sum' :: Num a => [a] -> a -- function declaration (always do this)
sum' [] = 0
sum' (x : []) = x + sum' [] -- xs is [] for a singleton list, as such the method returns x + sum' [] which is x + 0 or just x
