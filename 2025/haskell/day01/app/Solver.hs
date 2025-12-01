module Solver where

import Control.Monad.Trans.State
import Data.Maybe (mapMaybe)
import Prelude hiding (Left, Right)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: String -> Int
part1 = password1 . parse

part2 :: String -> Int
part2 = password2 . parse

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

newtype Position = Position Int deriving (Show)

data Rotation = Left Int | Right Int
  deriving (Show)

-- >>> rotate (Right 8) (Position 11)
-- Position 19
-- >>> rotate (Left 10) (Position 5)
-- Position 95
rotate :: Rotation -> Position -> Position
rotate (Left l) (Position p) = Position $ (p - l) `mod` 100
rotate (Right r) (Position p) = Position $ (p + r) `mod` 100

password1 :: [Rotation] -> Int
password1 rotations = evalState (password1Aux rotations) (Position 50)
  where
    password1Aux :: [Rotation] -> State Position Int
    password1Aux [] = return 0
    password1Aux (r : rs) = do
      rotateDial r
      thisCount <- countZeroes
      otherCounts <- password1Aux rs
      return $ thisCount + otherCounts
    countZeroes :: State Position Int
    countZeroes = do
      p <- get
      return $ if atOrigin p then 1 else 0
    atOrigin :: Position -> Bool
    atOrigin (Position 0) = True
    atOrigin _ = False

password2 :: [Rotation] -> Int
password2 rotations = evalState (password2Aux rotations) (Position 50)
  where
    password2Aux :: [Rotation] -> State Position Int
    password2Aux [] = return 0
    password2Aux (r : rs) = do
      thisCount <- countZeroes r
      rotateDial r
      otherCounts <- password2Aux rs
      return $ thisCount + otherCounts
    countZeroes :: Rotation -> State Position Int
    countZeroes (Right r) = do
      (Position p) <- get
      return $ (p + r) `div` 100
    countZeroes (Left l) = do
      (Position p) <- get
      return $ case p of
        0 -> l `div` 100
        _ -> (100 - p + l) `div` 100

rotateDial :: Rotation -> State Position ()
rotateDial r = modify (rotate r)

-- >>> parse "L68\nL30\nR48\n\n"
-- [Left 68,Left 30,Right 48]
parse :: String -> [Rotation]
parse = mapMaybe parseLine . lines
  where
    parseLine :: String -> Maybe Rotation
    parseLine ('L' : n) = Just $ Left $ read n
    parseLine ('R' : n) = Just $ Right $ read n
    parseLine _ = Nothing
