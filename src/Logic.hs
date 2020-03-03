module Logic
    -- ( calc
    -- )
      where

calc :: Show a => a -> IO ()
calc = putStrLn . show
