module P14 (p14_test) where

import Test.HUnit
import Data.List

dupli_rec :: [a] -> [a]
dupli_rec [] = []
dupli_rec (a:as) = (a : []) ++ (a : []) ++ dupli_rec as

dupli_rec_2 :: [a] -> [a]
dupli_rec_2 [] = []
dupli_rec_2 (a:as) = doubl a ++ dupli_rec_2 as
 where
  doubl a = a : (a : [])

{- wrong -}
dupli_rec_3 :: [a] -> [a]
dupli_rec_3 [] = []
{-dupli_rec_3 lst = [ x | x <- (lst ++ lst) ] -}
dupli_rec_3 lst = concat $ [ y : [y] | y <- lst ]


dupli_fold l = foldl (\acc b -> acc ++ [b,b]) [] l

dupli_map l = fst ( (mapAccumL (\acc b -> (acc ++ [b,b],b)) [] l) )


-- Tests


test_dupli_rec = TestCase $ do
 assertEqual "for (dupli_rec [1..3])" [1,1,2,2,3,3] (dupli_rec [1..3])
 assertEqual "for (dupli_rec [1])" [1,1] (dupli_rec [1])
 assertEqual "for (dupli_rec [])" 0 (length (dupli_rec []))
 assertEqual "for (dupli_rec 'abc')" ['a','a','b','b','c','c'] (dupli_rec "abc")

test_dupli_rec_2 = TestCase $ do
 assertEqual "for (dupli_rec [1..3])" [1,1,2,2,3,3] (dupli_rec_2 [1..3])
 assertEqual "for (dupli_rec [1])" [1,1] (dupli_rec_2 [1])
 assertEqual "for (dupli_rec [])" 0 (length (dupli_rec_2 []))
 assertEqual "for (dupli_rec 'abc')" ['a','a','b','b','c','c'] (dupli_rec_2 "abc")

test_dupli_rec_3 = TestCase $ do
 assertEqual "for (dupli_rec [1..3])" [1,1,2,2,3,3] (dupli_rec_3 [1..3])
 assertEqual "for (dupli_rec [1])" [1,1] (dupli_rec_3 [1])
 assertEqual "for (dupli_rec [])" 0 (length (dupli_rec_3 []))
 assertEqual "for (dupli_rec 'abc')" ['a','a','b','b','c','c'] (dupli_rec_3 "abc")

test_dupli_fold = TestCase $ do
 assertEqual "for (dupli_fold [1,2,3])" [1,1,2,2,3,3] (dupli_fold [1,2,3])

test_dupli_map = TestCase $ do
 assertEqual "for (dupli_map [1,2,3])" [1,1,2,2,3,3] (dupli_map [1,2,3])

p14_test = do
 runTestTT p14_tests

p14_tests = TestList [
 TestLabel "test_dupli_rec" test_dupli_rec,
 TestLabel "test_dupli_rec_2" test_dupli_rec_2,
 TestLabel "test_dupli_rec_3" test_dupli_rec_3,
 TestLabel "test_dupli_fold" test_dupli_fold,
 TestLabel "test_dupli_map" test_dupli_map
 ]
