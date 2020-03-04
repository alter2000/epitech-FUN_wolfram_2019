module PeanoNum (
  -- no constructors, transparent Nums
    PeanoNum
  , toNat
  , fromNat
  ) where


-- (Z)ero or a list of 1 + (Zero or a list of 1 + ...)
data PeanoNum = Z | S PeanoNum

toNat :: Integral a => a -> PeanoNum
toNat 0 = Z
toNat n
  | n < 0     = error "cannot happen lol"
  | otherwise = S . toNat $ n - 1

fromNat :: (Integral a, Num a, Eq a) => PeanoNum -> a
fromNat Z = 0
fromNat (S n) = 1 + fromNat n

instance Read PeanoNum where
  readsPrec _ ('-':_) = fail ""
  readsPrec _ n       = return (toNat $ read n, "")

instance Show PeanoNum where
  show = show . fromNat

instance Num PeanoNum where
  Z + n = n
  n + Z = n
  S n1 + S n2 = S . S $ n1 + n2

  fromInteger 0 = Z
  fromInteger i
    | i > 0 = S . fromInteger $ i - 1
    | otherwise = error "PeanoNum.fromInteger: positive numbers only."

  Z * _ = Z
  _ * Z = Z
  (S Z) * n = n
  n * (S Z) = n
  (S n1) * (S n2) = S . S $ n1 * n2

  abs = id

  signum = error "PNat is never negative"
  negate = error "PNat is never negative"
