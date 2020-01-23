data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

-- Infinite Tree
repeatT :: a -> Tree a
repeatT x = Node (repeatT x) x (repeatT x)

-- Takes the top n levels of the tree
takeT :: Int -> Tree a -> Tree a
takeT 0 t = Leaf
takeT _ Leaf = Leaf
takeT n (Node l x r) = Node (takeT (n - 1) l) x (takeT (n - 1) r)

-- creates a tree of height n; each node has the value x.
replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT
