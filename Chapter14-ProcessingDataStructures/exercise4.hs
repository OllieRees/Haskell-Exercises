import Data.Monoid
import Data.Foldable
import Data.Traversable

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  fmap f (Leaf) = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance (Monoid a) => Monoid (Tree a) where
  mempty = Leaf

  Leaf `mappend` my = my
  mx `mappend` Leaf = mx
  (Node l1 x r1) `mappend` (Node l2 y r2) = Node (l1 `mappend` l2) (x `mappend` y) (r1 `mappend` r2)

instance Foldable Tree where
  fold t = foldMap (id) t

  foldMap f (Leaf) = mempty
  foldMap f (Node l x r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

  foldl _ v (Leaf) = v
  foldl f v (Node l x r) = f (foldl f (foldl f v l) r) x

  foldr _ v (Leaf) = v
  foldr f v (Node l x r) = f x (foldr f (foldr f v r) l)

instance Traversable Tree where
  traverse f (Leaf) = pure Leaf
  traverse f (Node l x r) = pure Node <*> traverse f l <*> f x <*> traverse f r

