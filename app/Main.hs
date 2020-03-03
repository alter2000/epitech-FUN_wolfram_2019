module Main where

import System.Environment ( getArgs )
import Parsing ( parse, except )
import Logic ( calc )

main :: IO ()
main = do
  args <- getArgs
  case parse args of
    Left a -> except a
    Right b -> calc b
