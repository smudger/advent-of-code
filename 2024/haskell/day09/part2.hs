module Main where

import Data.Char (digitToInt)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve = checksum . defrag . toBlocks

data Content = File Int | Free deriving (Show)

type Block = (Content, Int)

type Input = String

-- >>> toBlocks "123"
-- [(File 0,1),(Free,2),(File 1,3)]
toBlocks :: String -> [Block]
toBlocks = map toBlock . zip [0 ..]

-- >>> toBlock (1, '6')
-- (Free,6)
-- >>> toBlock (2, '4')
-- (File 1,4)
toBlock :: (Int, Char) -> Block
toBlock (pos, c)
  | pos `mod` 2 == 0 = (File (pos `div` 2), digitToInt c)
  | otherwise = (Free, digitToInt c)

-- >>> defrag [(File 997, 2), (Free, 3), (File 998, 4), (File 999, 2), (Free, 2)]
-- [(File 997,2),(File 999,2),(Free,1),(File 998,4),(Free,2),(Free,2)]
defrag :: [Block] -> [Block]
defrag [] = []
defrag (b@((File _), _) : bs) = b : defrag bs
defrag ((Free, n) : bs) = b' : defrag bs'
  where
    (b', bs') = plug n bs

-- >>> plug 3 [(File 998, 3), (File 999, 5), (Free, 4)]
-- ((File 998,3),[(File 999,5),(Free,4)])
plug :: Int -> [Block] -> (Block, [Block])
plug n bs = case (popLast (isFileNoGreaterThan n) bs) of
  (bs', Just (File i, m)) -> moveFile i n m bs'
  (bs', _) -> ((Free, n), bs')

-- >>> moveFile 999 5 2 []
-- ((File 999,2),[(Free,3)])
-- >>> moveFile 999 2 2 []
-- ((File 999,2),[])
moveFile :: Int -> Int -> Int -> [Block] -> (Block, [Block])
moveFile i holeSize fileSize bs
  | holeSize == fileSize = ((File i, holeSize), bs)
  | otherwise = ((File i, fileSize), (Free, holeSize - fileSize) : bs)

-- >>> popLast (isFileNoGreaterThan 3) [(File 998, 2), (File 999, 2), (Free, 2)]
-- ([(File 998,2),(Free,2),(Free,2)],Just (File 999,2))
popLast :: (Block -> Bool) -> [Block] -> ([Block], Maybe Block)
popLast f arr = go (reverse arr) []
  where
    go [] r = (r, Nothing)
    go (x@(_, l) : xs) r = if f x then ((reverse xs) ++ ((Free, l) : r), Just x) else go xs (x : r)

-- >>> isFileNoGreaterThan 5 (Free, 4)
-- False
-- >>> isFileNoGreaterThan 5 (File 999, 4)
-- True
-- >>> isFileNoGreaterThan 5 (File 999, 5)
-- True
-- >>> isFileNoGreaterThan 5 (File 999, 6)
-- False
isFileNoGreaterThan :: Int -> Block -> Bool
isFileNoGreaterThan n (File _, m) = m <= n
isFileNoGreaterThan _ _ = False

-- >>> checksum [(File 0, 2), (Free, 1), (File 9, 2)]
-- 63
checksum :: [(Content, Int)] -> Int
checksum = go 0
  where
    go n ((File i, l) : bs) = ((* i) . sum . take l $ [n ..]) + go (n + l) bs
    go n ((Free, l) : bs) = go (n + l) bs
    go _ [] = 0

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
