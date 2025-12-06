module Main (main) where

import Data.List (sort)
import Solver (parse1, parse2, solveAll)
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
assert_Part1Example1 = solveAll (parse1 example1) @?= 4277556

assert_Part2Example1 :: Assertion
assert_Part2Example1 = solveAll (parse2 example1) @?= 3263827

{----------------------------------------------------------------------------------------------------------------------
    Examples
----------------------------------------------------------------------------------------------------------------------}

example1 :: String
example1 = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "
