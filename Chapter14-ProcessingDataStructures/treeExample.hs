import Data.Monoid
import Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Read)

instance Foldable Tree where
  -- fold :: Tree a -> a
  fold (Leaf x) = x
  fold (Node l r) = fold l `mappend` fold r

  -- foldMap :: (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = (foldMap f l) `mappend` (foldMap f r)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Traversable Tree where
  --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Leaf x) = pure Leaf <*> g x
  traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r

