module Experimentation.P03 (p03_test) where

import Test.HUnit
import Data.Maybe
import Data.List

elementAt_rec :: Int -> [a] -> Maybe a
elementAt_rec b all@(a:as)
 | b == 0 = Just a
 | b > length all = Nothing
 | otherwise =  elementAt_rec (b - 1) as

elementAt_nat :: Int -> [a] -> Maybe a
elementAt_nat b a
 | b > length a = Nothing
 | otherwise = Just (a !! b)


-- Tests


test_elementAt_rec = TestCase $ do
 assertEqual "for (test_elementAt_rec 9 [1..10])" (Just 10) (elementAt_rec 9 [1..10])
 assertEqual "for (test_elementAt_rec 9 [0..10])" (Just 9) (elementAt_rec 9 [0..10])

test_elementAt_nat = TestCase $ do
 assertEqual "for (test_elementAt_nat 9 [1..10])" (Just 10) (elementAt_nat 9 [1..10])
 assertEqual "for (test_elementAt_nat 9 [0..10])" (Just 9) (elementAt_nat 9 [0..10])

p03_test = do
 runTestTT p03_tests

p03_tests = TestList [
 TestLabel "test_elementAt_rec" test_elementAt_rec,
 TestLabel "test_elementAt_nat" test_elementAt_nat
 ]
