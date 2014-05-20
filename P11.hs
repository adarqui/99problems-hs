module P11 (p11_test, rleM_rec_4, Rle(..)) where

import Test.HUnit
import P10

{-data Rle = Multiple Int Char | Single Char deriving (Show,Eq)
rleM :: [Char] -> [Rle]
rleM [] = []
rleM (a:as) = rleT : rleM (drop (len - 1) as) ++ []
	where
		len = (length $ filter (== a) as) + 1
		rleT = if len > 1 then (Multiple len a) else (Single a)
-}

data Rle a = Multiple Int a | Single a deriving (Show,Eq)

rleM :: (Eq a) => [a] -> [Rle a]
rleM [] = []
rleM (a:as) = rleT : rleM (drop (len - 1) as) ++ []
	where
		len = (length $ takeWhile (== a) as) + 1
		rleT = if len > 1 then (Multiple len a) else (Single a)


rleM' :: (Eq a) => [a] -> [Rle a]
rleM' [] = []
rleM' (a:as) = Single a : []



rleM_rec_2 :: (Eq a) => [(Int,a)] -> [Rle a]
rleM_rec_2 [] = []
rleM_rec_2 (a:as) = what ++ rleM_rec_2 as
	where
		len (x, _) = x
		char (_, x) = x
		actual_char = char a
		actual_len = len a
		what =
			if actual_len > 1 then [Multiple actual_len actual_char]
			else [Single actual_char]



rleM_rec_3 :: (Eq a) => [(Int,a)] -> [Rle a]
rleM_rec_3 [] = []
rleM_rec_3 (a:as) = convertTuple a ++ rleM_rec_4 as
	where
		convertTuple (len, char) =
			if len > 1 then [Multiple len char]
			else [Single char]





rleM_rec_4 :: (Eq a) => [(Int,a)] -> [Rle a]
rleM_rec_4 [] = []
rleM_rec_4 (a:as) = convertTuple a ++ rleM_rec_4 as
	where convertTuple (len, char)
			| len > 1 = [Multiple len char]
			| otherwise = [Single char]




{-
data Rlex a = Multiplex Int a | Singlex a deriving (Show,Eq)
rlexM :: (Eq a) => [a] -> [Rlex a]
rlexM [] = []
rlexM (a:as) = rlexT : rlexM (drop (len - 1) as) ++ []
	where
		len = (length $ filter (== a) as) + 1
		rlexT = if len > 1 then (Multiplex len a) else (Singlex a)
-}

{-
main :: IO ()
main = do
	print $ rleM "aaabbcdefggga"
	print $ rleM' "aaabbcdefggga"
	print "hi"


-}

p11_test = do
	runTestTT p11_tests

test_rleM_rec = TestCase $ do
	assertEqual "for (rleM 'aaabbcda')" [Multiple 3 'a', Multiple 2 'b', Single 'c', Single 'd', Single 'a'] (rleM "aaabbcda")

test_rleM_rec_2 = TestCase $ do
	assertEqual "for (rleM_rec_2 $ rle_rec 'aaabbcda')" [Multiple 3 'a', Multiple 2 'b', Single 'c', Single 'd', Single 'a'] (rleM_rec_2 (rle_rec "aaabbcda"))


test_rleM_rec_3 = TestCase $ do
	assertEqual "for (rleM_rec_3 $ rle_rec 'aaabbcda')" [Multiple 3 'a', Multiple 2 'b', Single 'c', Single 'd', Single 'a'] (rleM_rec_3 (rle_rec "aaabbcda"))



test_rleM_rec_4 = TestCase $ do
	assertEqual "for (rleM_rec_4 $ rle_rec 'aaabbcda')" [Multiple 3 'a', Multiple 2 'b', Single 'c', Single 'd', Single 'a'] (rleM_rec_4 (rle_rec "aaabbcda"))




p11_tests = TestList [
				TestLabel "test_rleM_rec" test_rleM_rec,
				TestLabel "test_rleM_rec_2" test_rleM_rec_2,
				TestLabel "test_rleM_rec_3" test_rleM_rec_3,
				TestLabel "test_rleM_rec_4" test_rleM_rec_4
			]

