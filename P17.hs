module P17 (p17_test) where

import Test.HUnit
import Data.List


{-split_rec :: [a] -> Int -> [(a),(a)]-}
{-split_rec (a:as) n = [(as),(as)]-}
split_rec l n = [(c),(d)]
 where
  c = [ l !! nn | nn <- [0..(n-1)] ]
  d = [ l !! nn | nn <- [n..(length l - 1)] ]

split_nat l n = match $ f l n
 where
  f l n = splitAt n l
  match (a,b) = [(a),(b)]

-- Tests

test_split_rec = TestCase $ do
 assertEqual "for (split_rec 'abcdefghik') 3" [("abc"),("defghik")] (split_rec "abcdefghik" 3)

test_split_nat = TestCase $ do
 assertEqual "for (split_rec 'abcdefghik') 3" [("abc"),("defghik")] (split_rec "abcdefghik" 3)

p17_test = do
 runTestTT p17_tests


p17_tests = TestList [
 TestLabel "test_split_rec" test_split_rec,
 TestLabel "test_split_nat" test_split_nat
 ]
