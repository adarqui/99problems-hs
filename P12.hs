module P12 (p12_test) where

import Test.HUnit
import P11


decode_rleM_rec :: [Rle a] -> [a]
decode_rleM_rec [] = []
decode_rleM_rec (a:as) = what ++ decode_rleM_rec as
	where
		char [Single x] = x : []
		char [Multiple b x] = (replicate b x)
		what = char (a : [])



p12_test = do
	runTestTT p12_tests

test_decode_rleM_rec = TestCase $ do
	assertEqual "for (decode_rleM_rec [Multiple 3 'a', Multiple 2 'b', Single 'c', Single 'd', Single 'a'])" "aaabbcda" (decode_rleM_rec [Multiple 3 'a', Multiple 2 'b', Single 'c', Single 'd', Single 'a'])


p12_tests = TestList [
				TestLabel "test_decode_rleM_rec" test_decode_rleM_rec
			]

