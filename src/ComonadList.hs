{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{- TODO: Infinite list implementation for full type safety -}

module ComonadList
  -- (
  -- ..
  -- )
    where

--                        `-XKindSignatures`
class Functor c => Comonad (c :: * -> *) where
  extract :: c a -> a

  duplicate :: c a -> c (c a)
  duplicate = extend id

  extend :: (c a -> b) -> c a -> c b
  extend f = fmap f . duplicate

-- redefinition of Control.Comonad.=>>
(=>>) :: Comonad c => c a -> (c a -> b) -> c b
(=>>) = flip extend

data Cycle a = Cycle Int a a [a]
  deriving Functor

instance Comonad Cycle where
  extract (Cycle _ _ x _) = x
  duplicate x@(Cycle n _ _ _) = fromList $ take n $ iterate shift x
    where
      shift (Cycle a _ b (r:rs)) = Cycle a b r rs
      -- shift (Cycle _ _ _ []) = error "ComonadList.shift: infinite list not infinite?"

view :: Cycle a -> [a]
view (Cycle n _ x r) = take n (x:r)

fromList :: [a] -> Cycle a
-- NOP match
fromList []  = let a = a in Cycle 0 a a $ repeat a
fromList lst = let x:r = cycle lst
               in Cycle (length lst) (last lst) x r
