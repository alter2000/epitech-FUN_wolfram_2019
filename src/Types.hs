module Types (
    Opts (..)
  , module PeanoNum
  ) where

import PeanoNum ( PeanoNum )
import Control.Exception ( Exception )

data Opts = Opts
  { rule   :: !PeanoNum
  , start  :: !PeanoNum
  , lines  :: !PeanoNum
  -- , lines  :: !(Maybe PeanoNum)  -- maybe none or 0?
  , window :: !PeanoNum
  , move   :: !Integer
  } deriving (Show, Read)
