module PeanoNum (
    PeanoNum -- no constructors, transparent Nums
  , toNat
  , fromNat
  ) where


-- (Z)ero or a list of 1 + (Zero or a list of 1 + ...)
data PeanoNum = Z | S PeanoNum

toNat :: Integral a => a -> PeanoNum
toNat 0 = Z
toNat n = S . toNat $ n - 1

fromNat :: Integral a => PeanoNum -> a
fromNat Z = 0
fromNat (S n) = 1 + (fromNat n)

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
  fromInteger i | i > 0 = S . fromInteger $ i - 1

  Z * _ = Z
  _ * Z = Z
  (S Z) * n = n
  n * (S Z) = n
  (S n1) * (S n2) = S . S $ n1 * n2

  abs = id

  signum = error "PNat is never negative"
  negate = error "PNat is never negative"
