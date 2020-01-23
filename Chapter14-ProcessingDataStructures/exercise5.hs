filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p t = foldMap (\x -> if p x then x else mempty) t
