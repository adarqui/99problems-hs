module P05 (p05_test) where

import Test.HUnit
import Data.List

reverse_rec :: [a] -> [a]
reverse_rec [] = []
reverse_rec (a:as) = reverse_rec as ++ [a]

reverse_nat :: [a] -> [a]
reverse_nat all = reverse all


reverse_fold l = foldl' (\x y -> y : x) [] l

reverse_fold_2 l = foldr (\x y -> y ++ [x]) [] l


-- Tests


test_reverse_rec = TestCase $ do
	assertEqual "for (reverse_rec [1..5])" [5,4,3,2,1] (reverse_rec [1..5])
	assertEqual "for (reverse_rec [1])" [1] (reverse_rec [1])
	assertEqual "for (reverse_rec [])" 0 (length (reverse_rec []))

test_reverse_nat = TestCase $ do
	assertEqual "for (reverse_nat [1..5])" [5,4,3,2,1] (reverse_nat [1..5])
	assertEqual "for (reverse_nat [1])" [1] (reverse_nat [1])
	assertEqual "for (reverse_nat [])" 0 (length (reverse_nat []))

test_reverse_fold = TestCase $ do
	assertEqual "for (reverse_fold [1..5])" [5,4,3,2,1] (reverse_fold [1..5])

test_reverse_fold_2 = TestCase $ do
	assertEqual "for (reverse_fold_2 [1..5])" [5,4,3,2,1] (reverse_fold_2 [1..5])


p05_test = do
	runTestTT p05_tests


p05_tests = TestList [
				TestLabel "test_reverse_rec" test_reverse_rec,
				TestLabel "test_reverse_nat" test_reverse_nat,
				TestLabel "test_reverse_fold" test_reverse_fold,
				TestLabel "test_reverse_fold_2" test_reverse_fold_2
			]
