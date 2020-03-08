module Logic
  where

import ComonadList
import Types hiding ( rule
                    , start
                    , lines
                    , window
                    , move
                    )
import PeanoNum ( fromNat )
import Data.Function ((&))

initial :: Num a => PeanoNum -> [a] -> Int -> Cycle a
initial n lst w = padRight n lst
                & center n w
                & fromList

padRight :: Num a => PeanoNum -> [a] -> [a]
padRight f xs = take (fromNat f) $ xs ++ repeat 0
-- TODO: what is the center?
center :: PeanoNum -> Int -> [a] -> [a]
center n w = take (fromNat n) . drop ((fromNat n) `div` 2 - w) . cycle

-- black magic rule definition
rule :: (Integral a, Integral b) => a -> b -> b -> b -> a
rule n l x r = n `div` (2^(4*l + 2*x + r)) `mod` 2

step :: (a -> a -> a -> b) -> Cycle a -> b
step f (Cycle _ l x (r:_)) = f l x r
step _ (Cycle _ _ _ []) = error "ComonadList.step: infinite list not infinite?"

runCA :: (t -> t -> t -> t) -> Cycle t -> [Cycle t]
runCA r = iterate (=>> step r)

chop 0 s ls = drop (fromNat s) ls
chop n s ls = drop (fromNat n) ls & take (fromNat n)

result :: (Eq a, Num a) =>
     (a -> b)
  -- ^ table converter
  -> (a -> a -> a -> a)
  -- ^ rule
  -> Cycle a
  -- ^ seed
  -> [[b]]
result sh r s = (fmap sh . view <$>) $ runCA r s
