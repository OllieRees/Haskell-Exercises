data Node = Node Road (Maybe Road)
data Label = A | B | C deriving (Show) -- A is top road, B is bottom road and C is vertical road
data Road = Road Int Node
type Path = [(Label, Int)]
data Section = Section { getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section] 


startToEnd :: RoadSystem
startToEnd = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = 
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        forwardPriceToB = priceB + b
        crossPriceToA = priceB + b + c
        crossPriceToB = priceA + a + c
        newPathToA = if forwardPriceToA <= crossPriceToA 
            then (A, a):pathA
            else (C, c):(B, b):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB
            then (B, b):pathB
            else (C, c):(A, a):pathA
    in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in if sum (map snd bestAPath) > sum (map snd bestBPath) 
            then reverse bestAPath
        else reverse bestBPath

main = do
    contents    <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map ( \[a, b, c] -> Section a b c ) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is " ++ pathString
    putStrLn $ "The cost of this path is " ++ show pathPrice