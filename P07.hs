module P07 (p07_test) where


import Test.HUnit
import Data.List


flatten_nat a = concat a


flatten_rec :: [[a]] -> [a]
flatten_rec [] = []
flatten_rec [[a]] = [a]
flatten_rec (a:as) = a ++ flatten_rec as



-- Tests

test_flatten_nat = TestCase $ do
	assertEqual "for (flatten_nat [[1,2],[3,4]])" [1,2,3,4] (flatten_nat [[1,2],[3,4]])

test_flatten_rec = TestCase $ do
	assertEqual "for (flatten_rec [[1,2],[3,4],[5,6]])" [1,2,3,4,5,6] (flatten_rec [[1,2],[3,4],[5,6]])
	assertEqual "for (flatten_rec [[1,2],[3,4]])" [1,2,3,4] (flatten_rec [[1,2],[3,4]])
--	assertEqual "for (flatten_rec [[1,2],[[3,4],[5,6],[7])" [1,2,3,4,5,6,7] (flatten_rec [[[1,2],[[3,4],[5,6],[7]])
	assertEqual "for (flatten_rec [])" 0 (length (flatten_rec []))


-- [[[1,3],[2,9]],[[1,3],[4,5]],[[8,9],[5,6]],[[7,8]]]


p07_test = do
	runTestTT p07_tests

p07_tests = TestList [
				TestLabel "test_flatten_nat" test_flatten_nat,
				TestLabel "test_flatten_rec" test_flatten_rec
			]
