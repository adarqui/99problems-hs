module Experimentation.P22 (p22_test) where

import Test.HUnit
import Data.List

range_rec :: (Ord a, Enum a) => a -> a -> [a]
range_rec a b
 | a > b = []
 | otherwise = a : range_rec (succ a) b

range_fold :: (Enum a) => a -> a -> [a]
range_fold a b = foldl (\x y -> x ++ [y]) [] [a..b]

-- Tests

test_range_rec = TestCase $ do
 assertEqual "for (range_rec 4 9)" [4,5,6,7,8,9] (range_rec 4 9)
 assertEqual "for (range_rec 'a' 'c')" "abc" (range_rec 'a' 'c')


test_range_fold = TestCase $ do
 assertEqual "for (range_fold 4 9)" [4,5,6,7,8,9] (range_fold 4 9)
 assertEqual "for (range_fold 'a' 'c')" "abc" (range_fold 'a' 'c')


p22_test = do
 runTestTT p22_tests


p22_tests = TestList [
 TestLabel "test_range_rec" test_range_rec,
 TestLabel "test_range_fold" test_range_fold
 ]
