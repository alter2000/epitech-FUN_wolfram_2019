module Parsing
  -- ( parse
  -- , except
  -- )
    where

import Types

import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitWith, ExitCode (..) )
-- import Control.Monad.ST :-> StateT primitive => generalized ReadP
-- Control.Applicative reexported from ReadP
import Text.ParserCombinators.ReadP

-- typedef-ish
type Parsed = ReadP

parse :: [String] -> Either String Opts
parse sss = case runParser of
  [(opts, "")] -> Right opts
  [] -> Left "Error parsing arguments."
  ((_, s):_)  -> Left . (++) "Ambiguous parse: " $ show s
  where
    runParser = readP_to_S parseOpts $ unwords sss

parseOpts :: Parsed Opts
parseOpts = Opts
  <$> parseRule
  <*> parseStart
  <*> parseLines
  <*> parseWindow
  <*> parseMove

parseRule :: Parsed PeanoNum
parseRule = numOpt "rule"

parseStart :: Parsed PeanoNum
parseStart = numOpt "start" <++ pure 0

parseLines :: Parsed PeanoNum
parseLines = numOpt "lines" <++ pure 0

parseWindow :: Parsed PeanoNum
parseWindow = numOpt "window" <++ pure 80

parseMove :: Parsed Integer
parseMove = numOpt "move" <++ pure 0

opt :: String -> Parsed String
opt s = string $ "--" ++ s

digit, space :: ReadP Char
digit = satisfy isDigit
space = satisfy isSpace

isDigit, isSpace :: Char -> Bool
-- not all whitespace
isSpace c = c == ' ' || c == '\t'
isDigit c = c >= '0' && c <= '9'

numOpt :: Read a => String -> Parsed a
numOpt s = skipSpaces
        *> opt s
        *> skipSpaces
        *> (read <$> munch1 isDigit)

except :: String -> IO a
except a = hPutStrLn stderr a >> exitWith (ExitFailure 84)
