module Types (
    Opts (..)
  , WRule (..)
  , module PeanoNum
  ) where

import PeanoNum ( PeanoNum )
import Control.Exception ( Exception )

instance Exception String

data Opts = Opts
  { rule   :: !WRule
  , start  :: !PeanoNum
  , lines  :: !PeanoNum
  , window :: !PeanoNum
  , move   :: !Integer
  } deriving (Show, Read)

data OpRule = Rule | Start | Lines | Window | Move

data WRule = R30 | R90 | R110
  deriving (Show, Enum)

instance Read WRule where
  readsPrec _ "30"  = return (R30, "")
  readsPrec _ "90"  = return (R90, "")
  readsPrec _ "110" = return (R110, "")
  readsPrec _ _     = fail ""
