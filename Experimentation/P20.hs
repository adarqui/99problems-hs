module Experimentation.P20 (p20_test, removeAt_nat_2) where

import Test.HUnit
import Data.List
import Data.Maybe


{- WRONG -}
removeAt_rec :: [a] -> Int -> (Maybe a,[a])
removeAt_rec lst@(l:ls) 0 = (Just l, ls)
removeAt_rec lst@(l:ls) n = removeAt_rec ls (n - 1)


removeAt_fold :: [a] -> Int -> (Maybe a,[a])
removeAt_fold lst n = decode (result) where
						decode (a,b,_) = (a,b)
						xfst (a,_,_) = a
						xsnd (_,b,_) = b
						xthird (_,_,c) = c

						result = foldl f (Nothing,[],1) lst
								where
								f a b
									| xthird a == n = (Just b, xsnd a, xthird a + 1)
									| otherwise = (xfst a, xsnd a ++ [b], xthird a + 1)


removeAt_nat :: [a] -> Int -> (a,[a])
removeAt_nat lst@(l:ls) n = merge where
 spl = splitAt (n-1) lst
 merge = (head (snd spl), fst spl ++ tail (snd spl))


removeAt_nat_2 :: [a] -> Int -> (a, [a])
removeAt_nat_2 lst n = (head sp2, sp1 ++ tail sp2)
 where (sp1, sp2) = splitAt (n-1) lst

-- Tests

test_removeAt_fold = TestCase $ do
 assertEqual "for (removeAt_fold 'abcd' 2)" (Just 'b', "acd") (removeAt_fold "abcd" 2)

test_removeAt_rec = TestCase $ do
 assertEqual "for (removeAt_rec 'abcd' 2)" (Just 'b', "acd") (removeAt_rec "abcd" 2)

test_removeAt_nat = TestCase $ do
 assertEqual "for (removeAt_nat 'abcd' 2)" ('b', "acd") (removeAt_nat "abcd" 2)

test_removeAt_nat_2 = TestCase $ do
 assertEqual "for (removeAt_nat_2 'abcd' 2)" ('b', "acd") (removeAt_nat_2 "abcd" 2)

p20_test = do
 runTestTT p20_tests

p20_tests = TestList [
 TestLabel "test_removeAt_fold" test_removeAt_fold,
 TestLabel "test_removeAt_nat" test_removeAt_nat,
 TestLabel "test_removeAt_nat_2" test_removeAt_nat_2
 ]
