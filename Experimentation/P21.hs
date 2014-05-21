module Experimentation.P21 (p21_test) where

import Test.HUnit
import Data.List


insertAt_rec :: (Eq n,Num n) => a -> [a] -> n -> [a]
insertAt_rec _ [] _ = []
insertAt_rec a lst 1 = a : lst
insertAt_rec a lst@(l:ls) n = l : insertAt_rec a ls (n - 1)

insertAt_nat a lst n = merge
 where
  spl = splitAt (n-1) lst
  merge = fst spl ++ (a : snd spl)

insertAt_nat_2 a lst n = sp1 ++ [a] ++ sp2
 where
  (sp1, sp2) = splitAt (n-1) lst

-- Tests

test_insertAt_rec = TestCase $ do
 assertEqual "for (insertAt_rec 'X' 'abcd') 2" "aXbcd" (insertAt_rec 'X' "abcd" 2)
 assertEqual "for (insertAt_rec 'X' 'abcd') 1" "Xabcd" (insertAt_rec 'X' "abcd" 1)


test_insertAt_nat = TestCase $ do
 assertEqual "for (insertAt_nat 'X' 'abcd') 2" "aXbcd" (insertAt_nat 'X' "abcd" 2)
 assertEqual "for (insertAt_nat 'X' 'abcd') 1" "Xabcd" (insertAt_nat 'X' "abcd" 1)


test_insertAt_nat_2 = TestCase $ do
 assertEqual "for (insertAt_nat_2 'X' 'abcd') 2" "aXbcd" (insertAt_nat_2 'X' "abcd" 2)
 assertEqual "for (insertAt_nat_2 'X' 'abcd') 1" "Xabcd" (insertAt_nat_2 'X' "abcd" 1)


p21_test = do
 runTestTT p21_tests

p21_tests = TestList [
 TestLabel "test_insertAt_rec" test_insertAt_rec,
 TestLabel "test_insertAt_nat" test_insertAt_nat,
 TestLabel "test_insertAt_nat_2" test_insertAt_nat_2
 ]
