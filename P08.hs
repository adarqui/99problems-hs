module P08 (p08_test) where

import Test.HUnit
import Data.List

compress_rec :: (Eq a) => [a] -> [a]
compress_rec [] = []
compress_rec (a:as) = [a] ++ (dropWhile (== a) (compress_rec as) )

-- wrong
compress_nat :: (Eq a) => [a] -> [a]
compress_nat all = nub all

compress_rec_2 :: (Eq a) => [a] -> [a]
compress_rec_2 [] = []
compress_rec_2 (a:as) = [a] ++ (dropWhile (== a) (compress_rec_2 as))


-- Tests

test_compress_rec = TestCase $ do
 assertEqual "for (compress_rec 'aaaaabccddddeefga')" "abcdefga" (compress_rec "aaaaabccddddeefga")

test_compress_nat = TestCase $ do
 assertEqual "for (compress_nat 'aaaaabccddddeefga')" "abcdefga" (compress_nat "aaaaabccddddeefga")

test_compress_rec_2 = TestCase $ do
 assertEqual "for (compress_rec_2 'aaaaabccddddeefga')" "abcdefga" (compress_rec_2 "aaaaabccddddeefga")

p08_test = do
 runTestTT p08_tests

p08_tests = TestList [
 TestLabel "test_compress_rec" test_compress_rec,
 TestLabel "test_compress_nat" test_compress_nat,
 TestLabel "test_compress_rec_2" test_compress_rec_2
 ]
