module Logic
    ( calc
    , printAll
    , rule
    , initial
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

calc :: Opts -> IO ()
calc (Opts ruleNo start ls window move) =
  printAll (fromNat ls)
           (rule $ fromNat ruleNo)
           (initial
             (fromNat window) [1]
             (fromInteger move)
             (fromNat start))

initial :: Num a => Int -> [a] -> Int -> Int -> Cycle a
initial n lst w s = fromList $ drop s $ center $ padRight n lst
  where
    padRight f xs = take f $ xs ++ repeat 0
    -- TODO: what is the center?
    center = take n . drop (n `div` 2+w) . cycle

-- black magic rule definition
rule :: (Integral a, Integral b) => a -> b -> b -> b -> a
rule n l x r = n `div` (2^(4*l + 2*x + r)) `mod` 2

step :: (a -> a -> a -> b) -> Cycle a -> b
step f (Cycle _ l x (r:_)) = f l x r
step _ (Cycle _ _ _ []) = error "ComonadList.step: infinite list not infinite?"

runCA :: (t -> t -> t -> t) -> Cycle t -> [Cycle t]
runCA r = iterate (=>> step r)

printAll :: (Eq a, Num a) => Int -> (a -> a -> a -> a) -> Cycle a -> IO ()
printAll n r st = mapM_ putStrLn $ take n result
  where result = fmap display . view <$> runCA r st
        display 0 = ' '
        display 1 = '*'
        display _ = '\0'
