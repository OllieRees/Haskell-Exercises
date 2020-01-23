import Data.Foldable
import Data.Traversable

data Mayb a = J a | N

-- I actually don't know if these are the decent definitions, especially for traverse. 

instance Functor Mayb where
  fmap f N = N
  fmap f (J x) = J (f x)

instance Monoid a => Monoid (Mayb a) where
  mempty = N

  N `mappend` my = my
  mx `mappend` N = mx
  (J x) `mappend` (J y) = J (x `mappend` y)

instance Foldable Mayb where
  --fold :: Mayb a -> a
  fold m = foldMap id m

  --foldMap :: (a -> mon) -> Mayb a -> mon
  foldMap f m = foldr (mappend . f) mempty m

  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr _ v N = v
  foldr f v (J x) = f x (foldr f v N)

  -- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
  foldl _ v (N) = v
  foldl f v (J x) = f (foldl f v N) x

instance Traversable Mayb where
  -- traverse :: (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ N = pure N
  traverse f (J x) = pure J <*> (f x)
