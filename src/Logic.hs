module Logic
    ( calc
    )
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

calc :: Opts -> IO ()
calc (Opts ruleNo start ls window move) =
  printAll ls
            start
           (rule $ fromNat ruleNo)
           (initial
             (fromNat window) [1]
             (fromInteger move))

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

printAll :: (Eq a, Num a) =>
            PeanoNum
         -> PeanoNum
         -> (a -> a -> a -> a)
         -> ComonadList.Cycle a
         -> IO ()
printAll n b r seed = result
                    & chop n b
                    & mapM_ putStrLn
  where
    chop :: PeanoNum -> PeanoNum -> [a] -> [a]
    chop 0 b = yeet b
    chop n b = take (fromNat n) . yeet b

    yeet :: PeanoNum -> [a] -> [a]
    yeet n = drop (fromNat n)

    result :: [[Char]]
    result = fmap display . view <$> runCA r seed

    display :: (Eq a, Num a) => a -> Char
    display 0 = ' '
    display 1 = '*'
    display _ = '\0'
