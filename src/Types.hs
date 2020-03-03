module Types where


import Control.Exception (Exception (..), SomeException (..))
import Control.Monad.ST
import PeanoNum

instance Exception String

data Opts = Opts
  { rule   :: !WRule
  , start  :: !PeanoNum
  , lines  :: !PeanoNum
  , window :: !PeanoNum
  , move   :: !Integer
  } deriving (Show, Read)

data WRule = R30 | R90 | R110
  deriving (Show, Read, Enum)

type Parser = ST [(String, String)] (IO ())
-- same as `type ReadS a = [(a, String)]`
