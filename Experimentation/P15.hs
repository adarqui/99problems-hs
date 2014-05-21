module Experimentation.P15 (p15_test) where

import Test.HUnit
import Data.List

repli_nat :: [a] -> Int -> [a]
repli_nat [] _ = []
repli_nat (a:as) n = take n (repeat a) ++ repli_nat as n


repli_rec :: [a] -> Int -> [a]
repli_rec [] _ = []
repli_rec (a:as) n = (take n (repli a)) ++ repli_rec as n


repli a = [a] ++ repli a

repli_fold :: [a] -> Int -> [a]
repli_fold l n = snd $
 foldl
  (\a b -> ((fst a)+1,(snd a)++[(l !! (fst a)) | i <- [1..n] ]))
  (0,[])
  l

repli_map :: [a] -> Int -> [a]
repli_map l n = concat $ snd $
 mapAccumL
  (\a b -> (a+1, (take n (repeat b))))
  0
  l

-- Tests

test_repli_nat = TestCase $ do
 assertEqual "for (repli_nat [1..3] 2)" [1,1,2,2,3,3] (repli_nat [1..3] 2)
 assertEqual "for (repli_nat [1] 4)" [1,1,1,1] (repli_nat [1] 4)
 assertEqual "for (repli_nat [])" 0 (length (repli_nat [] 5))
 assertEqual "for (repli_nat 'abc') 1" ['a','b','c'] (repli_nat "abc" 1)
 assertEqual "for (repli_nat 'abc') 0" [] (repli_nat "abc" 0)


test_repli_rec = TestCase $ do
 assertEqual "for (repli_rec [1..3] 2)" [1,1,2,2,3,3] (repli_rec [1..3] 2)
 assertEqual "for (repli_rec [1] 4)" [1,1,1,1] (repli_rec [1] 4)
 assertEqual "for (repli_rec [])" 0 (length (repli_rec [] 5))
 assertEqual "for (repli_rec 'abc') 1" ['a','b','c'] (repli_rec "abc" 1)
 assertEqual "for (repli_rec 'abc') 0" [] (repli_rec "abc" 0)


test_repli_fold = TestCase $ do
 assertEqual "for (repli_fold [1..3] 2)" [1,1,2,2,3,3] (repli_fold [1..3] 2)
 assertEqual "for (repli_fold [1] 4)" [1,1,1,1] (repli_fold [1] 4)
 assertEqual "for (repli_fold [])" 0 (length (repli_fold [] 5))
 assertEqual "for (repli_fold 'abc') 1" "abc" (repli_fold "abc" 1)
 assertEqual "for (repli_fold 'abc') 0" [] (repli_fold "abc" 0)

test_repli_map = TestCase $ do
 assertEqual "for (repli_map [1..3] 2)" [1,1,2,2,3,3] (repli_map [1..3] 2)
 assertEqual "for (repli_map [1] 4)" [1,1,1,1] (repli_map [1] 4)
 assertEqual "for (repli_map [])" 0 (length (repli_map [] 5))
 assertEqual "for (repli_map 'abc') 1" "abc" (repli_map "abc" 1)
 assertEqual "for (repli_map 'abc') 0" [] (repli_map "abc" 0)

p15_test = do
 runTestTT p15_tests

p15_tests = TestList [
 TestLabel "test_repli_nat" test_repli_nat,
 TestLabel "test_repli_rec" test_repli_rec,
 TestLabel "test_repli_fold" test_repli_fold,
 TestLabel "test_repli_map" test_repli_map
 ]
