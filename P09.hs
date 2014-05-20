module P09 (p09_test) where

import Test.HUnit
import Data.List

pack_rec :: (Eq a) => [a] -> [[a]]
pack_rec [] = []
pack_rec (a:as) = ([a] ++ (takeWhile (==a) as)) : (pack_rec (dropWhile (==a) as))

pack_nat all = all : []



-- Tests




p09_test = do
	runTestTT p09_tests


test_pack_rec = TestCase $ do
	assertEqual "for (pack_rec ['a','a','b','c','c','c','d','z','a','b','b'])" ["aa","b","ccc","d","z","a","bb"] (pack_rec ['a','a','b','c','c','c','d','z','a','b','b'])

test_pack_nat = TestCase $ do
	assertEqual "for (pack_rec ['a','a','b','c','c','c','d','z','a','b','b'])" ["aa", "b","ccc","d","z","a","bb"] (pack_rec ['a','a','b','c','c','c','d','z','a','b','b'])


p09_tests = TestList [
				TestLabel "test_pack_rec" test_pack_rec,
				TestLabel "test_pack_nat" test_pack_nat
			]
