instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ []     = []
  fmap f (x:xs) = (f x) : fmap f xs
