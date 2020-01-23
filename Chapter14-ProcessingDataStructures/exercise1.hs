import Data.Monoid

instance (Monoid a, Monoid b) => Monoid (a, b) where
  --mempty :: (Monoid m) => m 
  mempty = (mempty, mempty)

  --mappend :: (Monoid m) => (m, m) -> (m, m) -> (m, m)
  (x1, y1) `mappend` (x2, y2) = ((x1 `mappend` x2), (y1 `mappend` y2))

