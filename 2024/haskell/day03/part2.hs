module Main where

import Data.Char (isDigit)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Text.Regex.TDFA

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve = snd . foldl' handleInstruction (True, 0) . findValidInstructions
  where
    findValidInstructions s = getAllTextMatches (s =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)") :: [String]
    handleInstruction (_, r) "do()" = (True, r)
    handleInstruction (_, r) "don't()" = (False, r)
    handleInstruction (False, r) _ = (False, r)
    handleInstruction (True, r) i = (True, r + (foldl1 (*) $ nums i))

-- | 'nums' breaks a string up into a list of integers, which were delimited
-- by non-digit characters (as defined by 'isDigit'), trimming any non-digit characters
-- at the beginning and at the end. Implementation closely follows implementation
-- for 'words' in core library.
--
-- ==== __Examples__
--
-- >>> nums "mul(23,42)"
-- [23,42]
nums :: String -> [Int]
nums s = case dropWhile (not . isDigit) s of
  "" -> []
  s' -> (read n) : nums s''
    where
      (n, s'') = break (not . isDigit) s'

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
