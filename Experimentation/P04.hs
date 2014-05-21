module Experimentation.P04 (p04_test) where

import Test.HUnit
import Data.List

length_nat :: [a] -> Int
length_nat all = length all

length_rec :: [a] -> Int
length_rec [] = 0
length_rec (_:as) = 1 + length_rec as


-- Tests


test_length_nat = TestCase $ do
 assertEqual "for (length_nat [0..100])" 101 (length_nat [0..100])
 assertEqual "for (length_nat [1])" 1 (length_nat [1])
 assertEqual "for (length_nat [])" 0 (length_nat [])


test_length_rec = TestCase $ do
 assertEqual "for (length_rec [0..100])" 101 (length_rec [0..100])
 assertEqual "for (length_rec [1])" 1 (length_rec [1])
 assertEqual "for (length_rec [])" 0 (length_rec [])

p04_test = do
 runTestTT p04_tests

p04_tests = TestList [
 TestLabel "test_length_nat" test_length_nat,
 TestLabel "test_length_rec" test_length_rec
 ]
