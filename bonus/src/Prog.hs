module Prog where

import PeanoNum
import ComonadList
import Types hiding ( rule )
import Logic
import Img
import Data.Function ((&))

calc :: Opts -> IO ()
calc (Opts ruleNo start ls window move img)
  | img       = imgRun   ls start pRule inits ruleNo
  | otherwise = printAll ls start pRule inits
    where
      pRule = rule $ fromNat ruleNo
      inits = initial (fromNat window) [1] (fromInteger move)

printAll :: (Eq a, Num a) =>
            PeanoNum
            -- ^ last line to print
         -> PeanoNum
            -- ^ first line to print
         -> (a -> a -> a -> a)
            -- ^ rule
         -> Cycle a
            -- ^ seed
         -> IO ()
printAll n b r s = result cliDisplay r s
                    & chop n b
                    & mapM_ putStrLn

cliDisplay :: (Eq a, Num a) => a -> Char
cliDisplay 0 = ' '
cliDisplay 1 = '*'
cliDisplay _ = '\0'
