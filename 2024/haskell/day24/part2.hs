module Main where

import Data.Bits (shift, shiftR, testBit, (.&.), (.^.), (.|.))
import Data.Char (digitToInt)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List (isInfixOf, isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe

main :: IO ()
main = print . solve $ input

solve :: Input -> Int
solve i = bits (system i) "z"

type Input = String

data Gate = Value Int | And String String | Or String String | Xor String String deriving (Show)

type System = Map String Gate

isAnd :: Gate -> Bool
isAnd (And _ _) = True
isAnd _ = False

isOr :: Gate -> Bool
isOr (Or _ _) = True
isOr _ = False

isXor :: Gate -> Bool
isXor (Xor _ _) = True
isXor _ = False

gates :: System -> [String]
gates s = M.keys s

-- >>> from "abd" (Or "def" "abc")
-- False
from :: String -> Gate -> Bool
from g (Or a b) = g `isPrefixOf` a || g `isPrefixOf` b
from g (Xor a b) = g `isPrefixOf` a || g `isPrefixOf` b
from g (And a b) = g `isPrefixOf` a || g `isPrefixOf` b
from _ _ = False

show2d :: Int -> String
show2d n
  | length (show n) == 1 = "0" ++ (show n)
  | otherwise = show n

initialise :: System -> Int -> Int -> System
initialise s x y = foldl' (\s' i -> M.insert ("y" ++ show2d i) (if testBit y i then Value 1 else Value 0) (M.insert ("x" ++ show2d i) (if testBit x i then Value 1 else Value 0) s')) s [0 .. 44]

test :: System -> Int
test s = head $ filter (\i -> add s (2 ^ i) (2 ^ i) /= 2 ^ (i + 1)) [0 ..]

-- >>> wrongBits 33
-- [0,5]
wrongBits :: Int -> [Int]
wrongBits n = go n 0
  where
    go 0 _ = []
    go n' b = case testBit n' 0 of
      True -> b : (go (shiftR n' 1) (b + 1))
      False -> go (shiftR n' 1) (b + 1)

-- >>> bits (M.fromList [("x02", Value 1), ("x06", Value 1)]) "x"
-- 68
bits :: System -> String -> Int
bits s p = sum . map (bit s) . filter (isPrefixOf p) . M.keys $ s

add :: System -> Int -> Int -> Int
add s x y = bits (initialise s x y) "z"

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
  Xor l r -> calculate s l .^. calculate s r

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)

example3 :: String
example3 = $(makeRelativeToProject "example3.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
