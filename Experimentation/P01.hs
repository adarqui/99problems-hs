module Experimentation.P01 (last_rec, last_foldl', p01_test) where

import Control.Monad
import Data.List
import Test.HUnit

last_rec :: [x] -> Maybe x
last_rec [] = Nothing
last_rec [x] = Just x
last_rec (x:xs) = last_rec xs

last_rec' :: [x] -> x
last_rec' [] = error "Empty"
last_rec' [x] = x
last_rec' (x:xs) = last_rec' xs

last_rec_2 :: [x] -> Maybe x
last_rec_2 xs =
 case xs of
  [] -> Nothing
  [x] -> Just x
  (_:xs) -> last_rec_2 xs

last_nat :: [x] -> x
last_nat all = last all

last_nat_2 :: [x] -> x
last_nat_2 all = all !! (length all - 1)

last_nat_3 :: [x] -> x
last_nat_3 all = reverse all !! 0

last_nat_4 :: [x] -> x
last_nat_4 = head . reverse

last_foldl :: [x] -> x
last_foldl lst@(l:ls) = foldl (\a b -> b) l lst

last_foldl' :: [x] -> x
last_foldl' lst@(l:ls) = foldl' (\a b -> b) l lst

-- Tests

p01_test = do
 runTestTT p01_tests

assertEqualX :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqualX preface expected actual = unless (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++ "expected!" ++ show expected ++ "\n but got: " ++ show actual

test_last_rec = TestCase (assertEqualX "for (last_rec [1,2,3])" (Just 3) (last_rec [1,2,3]))
test_last_rec_one_elm = TestCase (assertEqualX "for (last_rec [1])" (Just 1) (last_rec [1]))
test_last_rec_2 = TestCase (assertEqualX "for (last_rec_2 [1,4,7,9]" (Just 9) (last_rec_2 [1,4,7,9]))
test_last_nat = TestCase (assertEqualX "for (last_nat [1,2,3,4,5])" 5 (last_nat [1,2,3,4,5]))
test_last_nat_2 = TestCase (assertEqualX "for (last_nat_2 [1,2,3,7,8]" 8 (last_nat_2 [1,2,3,7,8]))
test_last_nat_3 = TestCase (assertEqualX "for (last_nat_3 [1,2,9,9]" 9 (last_nat_3 [1,2,9,9]))
test_last_foldl = TestCase (assertEqualX "for (last_foldl [1,2,9,9]" 9 (last_foldl [1,2,9,9]))


p01_tests = TestList [
 TestLabel "test_last_rec" test_last_rec,
 TestLabel "test_last_rec_one_elm" test_last_rec_one_elm,
 TestLabel "test_last_rec_2" test_last_rec_2,
 TestLabel "test_last_nat" test_last_nat,
 TestLabel "test_last_nat_2" test_last_nat_2,
 TestLabel "test_last_nat_3" test_last_nat_3,
 TestLabel "test_last_foldl" test_last_foldl
 ]

{-
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
-}
