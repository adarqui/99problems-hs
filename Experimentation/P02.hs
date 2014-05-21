module Experimentation.P02 (
 p02_test,
 nextToLast_nat_3,
 nextToLast_foldl,
 nextToLast_foldl',
 nextToLast_rec
 ) where

import Data.Maybe
import Test.HUnit
import Data.List


nextToLast_nat :: [a] -> Maybe a
nextToLast_nat as
 | (length as > 1) = Just(as !! (length as - 2))
 | otherwise = Nothing


nextToLast_nat_2 :: [a] -> Maybe a
nextToLast_nat_2 [] = Nothing
nextToLast_nat_2 [a] = Nothing
nextToLast_nat_2 all = Just $ head $ tail $ reverse all


nextToLast_nat_3 :: [a] -> a
nextToLast_nat_3 all = last $ init all


nextToLast_rec :: [a] -> a
nextToLast_rec (x:y:[]) = x
nextToLast_rec (x:xs) = nextToLast_rec xs


nextToLast_foldl :: [a] -> a
nextToLast_foldl (x:y:z) = fst $ foldl (\x y -> (snd x, y)) (x,y) z


nextToLast_foldl' :: [a] -> a
nextToLast_foldl' (x:y:z) = fst $ foldl' (\x y -> (snd x, y)) (x,y) z

-- Tests


test_nextToLast_nat = TestCase $ do
 assertEqual "for (nextToLast_nat [1,2,3])" (Just 2) (nextToLast_nat [1,2,3])
 assertEqual "for" (Just 1) (nextToLast_nat [1,2])

test_nextToLast_nat_2 = TestCase $ do
 assertEqual "for (nextToLast_nat_2 [1..5])" (Just 4) (nextToLast_nat_2 [1..5])
 assertEqual "for (nextToLast_nat_2 [1..2])" (Just 1) (nextToLast_nat_2 [1..2])

test_nextToLast_nat_3 = TestCase $ do
 assertEqual "for (nextToLast_Bi_3 [1..100])" [99,5] $ (nextToLast_nat_3 [1,99,100] : []) ++ (nextToLast_nat_3 [1..6]) : []


p02_test = do
 runTestTT p02_tests


p02_tests = TestList [
 TestLabel "test_nextToLast_nat" test_nextToLast_nat,
 TestLabel "test_nextToLast_nat_2" test_nextToLast_nat_2,
 TestLabel "test_nextToLast_nat_3" test_nextToLast_nat_3
 ]
