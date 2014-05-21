module Experimentation.P16 (p16_test) where

import Test.HUnit
import Data.List

dropEvery_rec :: [a] -> Int -> [a]
dropEvery_rec [] _ = []
dropEvery_rec _ 0 = []
dropEvery_rec l n = [ l !! i | i <- [0..(length l - 1)], ((i+1) `mod` n) /= 0 ]


dropEvery_fold :: [a] -> Int -> [a]
dropEvery_fold [] _ = []
dropEvery_fold _ 0 = []
dropEvery_fold l n = snd $
 foldl
 (\a b ->
  if
   (( ((fst a)+1) `mod` n) /= 0)
  then
   ((fst a)+1, snd a ++ [b])
  else
   ((fst a)+1, snd a)
  )
 (0,[])
 l


dropEvery_fold_2 :: [a] -> Int -> [a]
dropEvery_fold_2 [] _ = []
dropEvery_fold_2 _ 0 = []
dropEvery_fold_2 l n = snd $
						foldl
							(\a b -> case () of
								_ | nmod /= 0 -> (np1, snda ++ [b])
								  | otherwise -> (np1, snda)
								  where
								  	snda = snd a
								  	np1 = (fst a) + 1
								  	nmod = (np1 `mod` n)
							)
							(0,[])
							l

-- Tests


{-
 abdeghk
 abdfhk
 0 1 2 3 4 5 6 7 8 9
 a b | d e | g h | k
 a b | d | f | h | k

 a b c | e f | h i |
-}

test_dropEvery_rec = TestCase $ do
 assertEqual "for (dropEvery_rec 'abcdefghik')" "abdeghk" (take 10 (dropEvery_rec "abcdefghik" 3))
 assertEqual "for (dropEvery_rec [])" 0 (length (dropEvery_rec [] 5))
 assertEqual "for (dropEvery_rec 'abc') 1" [] (dropEvery_rec "abc" 1)
 assertEqual "for (dropEvery_rec 'abc') 0" [] (dropEvery_rec "abc" 0)


test_dropEvery_fold = TestCase $ do
 assertEqual "for (dropEvery_fold 'abcdefghik')" "abdeghk" (take 10 (dropEvery_fold "abcdefghik" 3))
 assertEqual "for (dropEvery_fold [])" 0 (length (dropEvery_fold [] 5))
 assertEqual "for (dropEvery_fold 'abc') 1" [] (dropEvery_fold "abc" 1)
 assertEqual "for (dropEvery_fold 'abc') 0" [] (dropEvery_fold "abc" 0)


test_dropEvery_fold_2 = TestCase $ do
 assertEqual "for (dropEvery_fold_2 'abcdefghik')" "abdeghk" (take 10 (dropEvery_fold_2 "abcdefghik" 3))
 assertEqual "for (dropEvery_fold_2 [])" 0 (length (dropEvery_fold_2 [] 5))
 assertEqual "for (dropEvery_fold_2 'abc') 1" [] (dropEvery_fold_2 "abc" 1)
 assertEqual "for (dropEvery_fold_2 'abc') 0" [] (dropEvery_fold_2 "abc" 0)


p16_test = do
 runTestTT p16_tests


p16_tests = TestList [
 TestLabel "test_dropEvery_rec" test_dropEvery_rec,
 TestLabel "test_dropEvery_fold" test_dropEvery_fold,
 TestLabel "test_dropEvery_fold_2" test_dropEvery_fold_2
 ]
