
twoGenerators :: [(Int, Int)]
twoGenerators = [(x, y) | x <- [1, 2], y <- [3, 4]]

oneGenerator :: [(Int, Int)]
oneGenerator = concat [ [(x, y) | y <- [3, 4]] | x <- [1, 2] ] -- Is this legit?