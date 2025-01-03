module Main where

import Data.Bits
import Data.Char (isDigit)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List

main :: IO ()
main = print . solve $ input

type Input = String

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: Input -> String
solve x = run program state
  where
    (state, program) = parse x

data State = MkState
  { registerA :: Int,
    registerB :: Int,
    registerC :: Int,
    ptr :: Int
  }

type Result = (State, Maybe String)

data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv

type Operand = Int

parse :: Input -> (State, [(Instruction, Operand)])
parse x =
  let (a : b : c : prog) = nums x
      parsePair (opcode : operand : _) = (instructionFromOpcode opcode, operand)
      parsePair _ = error "failed chunking"
   in (MkState a b c 0, map parsePair . chunksOf 2 $ prog)

instructionFromOpcode :: Int -> Instruction
instructionFromOpcode 0 = Adv
instructionFromOpcode 1 = Bxl
instructionFromOpcode 2 = Bst
instructionFromOpcode 3 = Jnz
instructionFromOpcode 4 = Bxc
instructionFromOpcode 5 = Out
instructionFromOpcode 6 = Bdv
instructionFromOpcode 7 = Cdv
instructionFromOpcode _ = error "unknown opcode"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : chunksOf n (drop n xs)

nums :: String -> [Int]
nums s = case dropWhile (not . liftA2 (||) isDigit (== '-')) s of
  "" -> []
  s' -> (read n) : nums s''
    where
      (n, s'') = break (not . liftA2 (||) isDigit (== '-')) s'

run :: [(Instruction, Operand)] -> State -> String
run program = loop ""
  where
    loop :: String -> State -> String
    loop output state = case (program !? ((ptr state) `div` 2)) of
      Nothing -> drop 1 output
      Just (instruction, operand) ->
        let (state', out) = process instruction operand state
            output' = case out of
              Nothing -> output
              Just out' -> output ++ "," ++ out'
         in loop output' state'

process :: Instruction -> Operand -> State -> Result
process Adv operand state =
  let a' = (registerA state) `div` (2 ^ (combo operand state))
   in ((advancePtr state) {registerA = a'}, Nothing)
process Bxl operand state =
  let b' = (registerB state) `xor` operand
   in ((advancePtr state) {registerB = b'}, Nothing)
process Bst operand state =
  let b' = (combo operand state) `mod` 8
   in ((advancePtr state) {registerB = b'}, Nothing)
process Jnz operand state = case (registerA state) of
  0 -> (advancePtr state, Nothing)
  _ -> (state {ptr = operand}, Nothing)
process Bxc _ state =
  let b' = (registerB state) `xor` (registerC state)
   in ((advancePtr state) {registerB = b'}, Nothing)
process Out operand state =
  let out = (combo operand state) `mod` 8
   in (advancePtr state, Just (show out))
process Bdv operand state =
  let b' = (registerA state) `div` (2 ^ (combo operand state))
   in ((advancePtr state) {registerB = b'}, Nothing)
process Cdv operand state =
  let c' = (registerA state) `div` (2 ^ (combo operand state))
   in ((advancePtr state) {registerC = c'}, Nothing)

advancePtr :: State -> State
advancePtr state = state {ptr = (ptr state) + 2}

combo :: Operand -> State -> Int
combo 0 _ = 0
combo 1 _ = 1
combo 2 _ = 2
combo 3 _ = 3
combo 4 s = registerA s
combo 5 s = registerB s
combo 6 s = registerC s
combo _ _ = error "unknown operand"
