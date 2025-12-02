module Main (main) where

import Data.List (sort)
import Solver (parse, part1, part2)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

{----------------------------------------------------------------------------------------------------------------------
    Main
----------------------------------------------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests]

{----------------------------------------------------------------------------------------------------------------------
    Property Tests
----------------------------------------------------------------------------------------------------------------------}

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests"
    [ {--
        Invariants
      --}
      {--
        Postconditions
      --}
      {--
        Metamorphic properties
      --}
      testProperty "sort == sort . reverse" prop_SortReverse
      {--
        Inductive properties
      --}
      {--
        Model-based properties
      --}
    ]

prop_SortReverse :: [Int] -> Bool
prop_SortReverse xs = sort xs == sort (reverse xs)

{----------------------------------------------------------------------------------------------------------------------
    Unit Tests
----------------------------------------------------------------------------------------------------------------------}

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "It solves part1 using example1" assert_Part1Example1,
      testCase "It solves part2 using example1" assert_Part2Example1
    ]

assert_Part1Example1 :: Assertion
assert_Part1Example1 = part1 (parse example1) @?= 1227775554

assert_Part2Example1 :: Assertion
assert_Part2Example1 = part2 (parse example1) @?= 4174379265

{----------------------------------------------------------------------------------------------------------------------
    Examples
----------------------------------------------------------------------------------------------------------------------}

example1 :: String
example1 = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"