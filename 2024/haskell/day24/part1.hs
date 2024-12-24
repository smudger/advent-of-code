module Main where

import Data.Bits (shift, xor, (.&.), (.|.))
import Data.Char (digitToInt)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List (isInfixOf, isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe

main :: IO ()
main = print . solve $ input

solve :: Input -> Int
solve i = sum . map (bit s) . filter (isPrefixOf "z") . M.keys $ s
  where
    s = system i

type Input = String

data Gate = Value Int | And String String | Or String String | Xor String String deriving (Show)

type System = Map String Gate

-- >>> bit (M.fromList [("z02", Value 1)]) "z02"
-- 4
bit :: System -> String -> Int
bit s g = shift (calculate s g) (read $ drop 1 g)

-- >>> system "x00: 1\nx01: 1"
-- fromList [("x00",Value 1),("x01",Value 1)]
system :: Input -> System
system = M.fromList . mapMaybe gate . lines

-- >>> gate "x00: 1"
-- Just ("x00",Value 1)
-- >>> gate "x00 AND y00 -> z00"
-- Just ("z00",And "x00" "y00")
-- >>> gate ""
-- Nothing
gate :: String -> Maybe (String, Gate)
gate g = case (": " `isInfixOf` g, "AND" `isInfixOf` g, "XOR" `isInfixOf` g, "OR" `isInfixOf` g) of
  (True, _, _, _) -> Just (take 3 g, Value (digitToInt $ last g))
  (_, True, _, _) -> Just (drop 15 g, And (take 3 g) (take 3 $ drop 8 g))
  (_, _, True, _) -> Just (drop 15 g, Xor (take 3 g) (take 3 $ drop 8 g))
  (_, _, _, True) -> Just (drop 14 g, Or (take 3 g) (take 3 $ drop 7 g))
  _ -> Nothing

-- >>> calculate (M.fromList [("abc", Value 1), ("def", Value 0), ("ghi", Or "abc" "def")]) "ghi"
-- 1
calculate :: System -> String -> Int
calculate s g = case s M.! g of
  Value v -> v
  And l r -> calculate s l .&. calculate s r
  Or l r -> calculate s l .|. calculate s r
  Xor l r -> calculate s l `xor` calculate s r

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
