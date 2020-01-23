-- From Haskell for Mac's tutorial page

-- Lists are defined as [a] = a:a:a:..:[] or [a] = a:[a]. This definition is recursive as the definition supposes that a list exists (either an empty 1`)
-- We can define List as a recursive type with a parameterised type constructor. 
data List a = a `Cons` (List a) | Nil

-- Another type that's recursively defined is a tree. Either it's a leaf, or it's a node with n trees branching off.
data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a 
insertTree n Leaf = Node n Leaf Leaf
insertTree n (Node root leftSubTree rightSubTree)
    | n < root      = Node root (insertTree n leftSubTree) rightSubTree 
    | otherwise     = Node root leftSubTree (insertTree n rightSubTree)

isElementTree :: Ord a => a -> BinaryTree a -> Bool
isElementTree _ Leaf = False
isElementTree n (Node root leftSubTree rightSubTree) 
    | n == root     = True
    | n < root      = isElementTree n leftSubTree
    | otherwise     = isElementTree n rightSubTree    

-- safeLength can be used to ensure safeHead and safeTail don't return Maybe a
safeLength :: [a] -> Maybe Int
safeLength xs = case len of
    0   -> Nothing
    _   -> Just len
    where
        len = length xs

safeHead :: [a] -> a
safeHead ls@(x:xs) = case safeLength ls of
    Nothing     -> error "No list given"
    _           -> x

safeTail :: [a] -> [a]
safeTail ls@(x:xs) = case safeLength ls of 
    Nothing     -> error "No list given"
    _           -> xs

myLength :: [a] -> Int
myLength l = case l of
    []      -> 0
    _       -> 1 + ( myLength tail )
    where
        tail = safeTail l 

deleteSorted :: Ord a => a -> [a] -> [a]
deleteSorted e ls@(x:xs) 
        | ls == []  = [] -- empty list exhausts options
        | e <= x    = xs -- return list without x
        | e > x     = x:( deleteSorted e xs ) -- append x on and try again with the tail

deleteSmallestTree :: Ord a => BinaryTree a -> BinaryTree a
deleteSmallestTree binTree = case binTree of
    Node a  Leaf          Leaf             -> Leaf
    Node a (leftSubTree) (rightSubTree)    -> Node a (deleteSmallestTree leftSubTree) rightSubTree  -- move down left subtree.

treeToList :: BinaryTree a -> [a]
treeToList binTree = case binTree of
    Leaf                                -> [] 
    Node a (Leaf) (Leaf)                -> [a]
    Node a (leftSubTree) (rightSubTree) -> a : ((treeToList leftSubTree) ++ (treeToList rightSubTree))

binTree :: BinaryTree Int
binTree = insertTree 1 $ insertTree 4 $ insertTree 9 $ insertTree 12 $ insertTree 6 $ insertTree 10 $ insertTree 5 $ insertTree 8 $ insertTree 7 Leaf