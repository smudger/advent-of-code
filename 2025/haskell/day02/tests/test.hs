module Main (main) where

import Data.List (nub)
import Solver (parse, part1, part2, solve)
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
      testProperty "inputs are greater than 0" prop_inputGreaterThanZero,
      testProperty "inputs do not contain duplicates" prop_inputNoDuplicates,
      {--
        Postconditions
      --}
      testProperty "part 1 result is less than the sum of the input" prop_part1ResultLessThanSum,
      testProperty "part 2 result is less than the sum of the input" prop_part2ResultLessThanSum,
      {--
        Metamorphic properties
      --}
      testProperty "part 1 result does not decrease when adding an int" prop_part1ResultNoDecrease,
      testProperty "part 2 result does not decrease when adding an int" prop_part2ResultNoDecrease
      {--
        Inductive properties
      --}
      {--
        Model-based properties
      --}
    ]

newtype Input = Input [Int]
  deriving (Show, Eq)

instance Arbitrary Input where
  arbitrary = sized $ \sz -> Input <$> nub <$> listOf (chooseInt (1, sz))
  shrink (Input i) = Input <$> shrink i

prop_inputGreaterThanZero :: Input -> Bool
prop_inputGreaterThanZero (Input i) = all (> 0) i

prop_inputNoDuplicates :: Input -> Property
prop_inputNoDuplicates (Input i) = length (nub i) === length i

prop_part1ResultLessThanSum :: Input -> Property
prop_part1ResultLessThanSum (Input i) =
  let totalSum = sum i
      part1Sol = part1 (solve i)
   in counterexample (show part1Sol ++ " > " ++ show totalSum) $ part1Sol <= totalSum

prop_part2ResultLessThanSum :: Input -> Property
prop_part2ResultLessThanSum (Input i) =
  let totalSum = sum i
      part2Sol = part2 (solve i)
   in counterexample (show part2Sol ++ " > " ++ show totalSum) $ part2Sol <= totalSum

prop_part1ResultNoDecrease :: Input -> Int -> Property
prop_part1ResultNoDecrease (Input is) i =
  let r = part1 (solve is)
      r' = part1 (solve (i : is))
   in counterexample (show r ++ " > " ++ show r') $ r <= r'

prop_part2ResultNoDecrease :: Input -> Int -> Property
prop_part2ResultNoDecrease (Input is) i =
  let r = part2 (solve is)
      r' = part2 (solve (i : is))
   in counterexample (show r ++ " > " ++ show r') $ r <= r'

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
assert_Part1Example1 =
  let sol1 = part1 . solve . parse $ example1
   in sol1 @?= 1227775554

assert_Part2Example1 :: Assertion
assert_Part2Example1 =
  let sol2 = part2 . solve . parse $ example1
   in sol2 @?= 4174379265

{----------------------------------------------------------------------------------------------------------------------
    Examples
----------------------------------------------------------------------------------------------------------------------}

example1 :: String
example1 = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"