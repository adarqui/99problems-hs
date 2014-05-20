module P19 (p19_test) where

import Test.HUnit
import Data.List

rotate_rec [] n = []
rotate_rec lst 0 = lst
rotate_rec lst@(l:ls) n
 | n > 0 = rotate_rec (ls ++ [l]) (n - 1)
 | otherwise = rotate_rec (last lst : init lst) (n + 1)


rotate_fold [] n = []
rotate_fold l n = foldl f l [1..abs n]
 where
  f all@(a:as) b
   | n < 0 = last all : init all
   | otherwise = as ++ [a]


rotate_fold_2 [] n = []
rotate_fold_2 l n = foldl fptr l [1..abs n]
 where
  fptr
   | n < 0 = f1
   | otherwise = f2
  f1 a b = last a : init a
  f2 (a:as) b = as ++ [a]


{- STRICT -}
rotate_fold' [] n = []
rotate_fold' l n = foldl' f l [1..abs n]
 where
  f all@(a:as) b
   | n < 0 = last all : init all
   | otherwise = as ++ [a]


-- Tests

test_rotate_rec = TestCase $ do
 assertEqual "for (rotate_rec 'abcdefgh' 3)" "defghabc" (rotate_rec "abcdefgh" 3)
 assertEqual "for (rotate_rec 'abcdefgh' 2)" "ghabcdef" (rotate_rec "abcdefgh" (-2))


test_rotate_fold = TestCase $ do
 assertEqual "for (rotate_fold 'abcdefgh' 3)" "defghabc" (rotate_fold "abcdefgh" 3)
 assertEqual "for (rotate_fold 'abcdefgh' 1)" "bcdefgha" (rotate_fold "abcdefgh" 1)
 assertEqual "for (rotate_fold 'abcdefgh' -2)" "ghabcdef" (rotate_fold "abcdefgh" (-2))
 assertEqual "for (rotate_fold 'abcdefgh' 0)" "abcdefgh" (rotate_fold "abcdefgh" 0)


test_rotate_fold_2 = TestCase $ do
 assertEqual "for (rotate_fold_2 'abcdefgh' 3)" "defghabc" (rotate_fold_2 "abcdefgh" 3)
 assertEqual "for (rotate_fold_2 'abcdefgh' 1)" "bcdefgha" (rotate_fold_2 "abcdefgh" 1)
 assertEqual "for (rotate_fold_2 'abcdefgh' -2)" "ghabcdef" (rotate_fold_2 "abcdefgh" (-2))
 assertEqual "for (rotate_fold_2 'abcdefgh' 0)" "abcdefgh" (rotate_fold_2 "abcdefgh" 0)


test_rotate_fold_again = TestCase $ do
 assertEqual "for (rotate_fold' 'ab' 20000)" "ab" (rotate_fold' "ab" 20000)

p19_test = do
 runTestTT p19_tests


p19_tests = TestList [
 TestLabel "test_rotate_rec" test_rotate_rec,
 TestLabel "test_rotate_fold" test_rotate_fold,
 TestLabel "test_rotate_fold_2" test_rotate_fold_2,
 TestLabel "test_rotate_fold_again" test_rotate_fold_again
 ]
