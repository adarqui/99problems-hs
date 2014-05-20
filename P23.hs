module P23 (p23_test) where

import Test.HUnit
import Data.List
import System.Random
import Control.Monad (replicateM)
import P20

-- getStdRandom $ randomR (1,5)
randSelect_rec :: [a] -> Int -> IO [a]
randSelect_rec [] _ = return []
randSelect_rec lst n = do
						h <- replicateM n $
							getStdRandom $ randomR (0, (length lst) - 1)
						return [lst !! p | p <- h ]



randSelect_rec_2 :: [a] -> Int -> IO [a]
randSelect_rec_2 [] _ = return []
randSelect_rec_2 l 0 = return l
randSelect_rec_2 lst@(l:ls) n = do
						h <- getStdRandom $ randomR (0, (length lst) - 1)
						z <- (randSelect_rec_2 ls (n - 1))
						return z
--						return [lst !! p | p <- h ]
{-[elm] ++ randSelect_rec (snd rest) n
							where
							rand_num = head $ take 1 $ randomRs (0, n) (getStdGen)
							elm = lst !! rand_num
							rest = removeAt_nat_2 lst rand_num
							result = ls
							-}

-- Tests

test_randSelect_rec = TestCase $ do
{-	g <- getStdGen
	setStdGen g -}
	res <- randSelect_rec "abcdefgh" 3
	assertEqual "for (randSelect_rec 'abcdefgh')" 3 (length res)

test_randSelect_rec_2 = TestCase $ do
	res <- randSelect_rec_2 "abcdefgh" 3
	assertEqual "for (reandSelect_rec_2 'abcdefgh')" [] res


p23_test = do
	runTestTT p23_tests


p23_tests = TestList [
				TestLabel "test_randSelect_rec" test_randSelect_rec,
				TestLabel "test_randSelect_rec_2" test_randSelect_rec_2
			]
