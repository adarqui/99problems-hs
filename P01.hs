module P01 (last_rec, p01_test, benchMark) where

import Control.Monad
import Data.List
import Test.HUnit
import Criterion.Main


last_rec :: [x] -> Maybe x
last_rec [] = Nothing
last_rec [x] = Just x
last_rec (x:xs) = last_rec xs

last_rec_2 :: [x] -> Maybe x
last_rec_2 xs = case xs of [] -> Nothing
                           [x] -> Just x
                           (_:xs) -> last_rec_2 xs

last_nat :: [x] -> x
last_nat all = last all

last_nat_2 :: [x] -> x
last_nat_2 all = all !! (length all - 1)

last_nat_3 :: [x] -> x
last_nat_3 all = reverse all !! 0

last_fold :: [x] -> x
last_fold lst@(l:ls) = foldl (\a b -> b) l lst


-- Tests



p01_test = do
	runTestTT p01_tests

assertEqualX :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqualX preface expected actual = 
  unless (actual == expected) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
              "expected!" ++ show expected ++ "\n but got: " ++ show actual

test_last_rec = TestCase (assertEqualX "for (last_rec [1,2,3])" (Just 3) (last_rec [1,2,3]))
test_last_rec_one_elm = TestCase (assertEqualX "for (last_rec [1])" (Just 1) (last_rec [1]))
--tt :: (Testable t) => String -> t -> Test
--tt a b = (TestCase a b)
--test_last_rec_fail :: Test
--test_last_rec_fail = TestCase (assertEqualX "for (last_rec [])" Nothing (last_rec []))
test_last_rec_2 = TestCase (assertEqualX "for (last_rec_2 [1,4,7,9]" (Just 9) (last_rec_2 [1,4,7,9]))
--test_last_rec_2_fail = TestCase (assertEqualX "for (last_rec_2_fail []" Nothing (last_rec_2 []))
test_last_nat = TestCase (assertEqualX "for (last_nat [1,2,3,4,5])" 5 (last_nat [1,2,3,4,5]))
test_last_nat_2 = TestCase (assertEqualX "for (last_nat_2 [1,2,3,7,8]" 8 (last_nat_2 [1,2,3,7,8]))
test_last_nat_3 = TestCase (assertEqualX "for (last_nat_3 [1,2,9,9]" 9 (last_nat_3 [1,2,9,9]))
test_last_fold = TestCase (assertEqualX "for (last_fold [1,2,9,9]" 9 (last_fold [1,2,9,9]))


p01_tests = TestList [
				TestLabel "test_last_rec" test_last_rec,
				TestLabel "test_last_rec_one_elm" test_last_rec_one_elm,
--				TestLabel "test_last_rec_fail" test_last_rec_fail,
				TestLabel "test_last_rec_2" test_last_rec_2,
--				TestLabel "test_last_rec_2_fail" test_last_rec_2_fail,
				TestLabel "test_last_nat" test_last_nat,
				TestLabel "test_last_nat_2" test_last_nat_2,
				TestLabel "test_last_nat_3" test_last_nat_3,
				TestLabel "test_last_fold" test_last_fold
			]


nn = 10000

benchMark = defaultMain [
			bgroup "last" [ bench "last_rec [1..10000]" $ whnf last_rec [1..nn],
						bench "last_rec_2 [1..10000]" $ whnf last_rec_2 [1..nn],
						bench "last_nat [1..10000]" $ whnf last_nat [1..nn],
						bench "last_nat_2 [1..10000]" $ whnf last_nat_2 [1..nn],
						bench "last_nat_3 [1..10000]" $ whnf last_nat_3 [1..nn],
						bench "last_fold [1..10000]" $ whnf last_fold [1..nn]
				]
			]
