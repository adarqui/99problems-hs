module P10 (p10_test, rle_rec) where

import Test.HUnit
import Data.List

rle_rec :: (Eq a) => [a] -> [(Int,a)]
rle_rec [] = []
rle_rec (a:as) = (b, a) : rle_rec (dropWhile (== a) as)
	where
		c = (takeWhile (== a) as)
		b = length (c) + 1


-- Tests


p10_test = do
	runTestTT p10_tests


test_rle_rec = TestCase $ do
	assertEqual "for (rle_rec aaabbc)" [(3,'a'),(2,'b'),(1,'c')] (rle_rec "aaabbc")

p10_tests = TestList [
				TestLabel "test_rle_rec" test_rle_rec
			]
