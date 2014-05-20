module P06 (p06_test) where

import Test.HUnit
import Data.List

isPal_rec :: (Eq a) => [a] -> Bool
isPal_rec [] = False
isPal_rec all = [head all] ++ tail all == reverse all


-- Tests


test_isPal_rec = TestCase $ do
 assertEqual "for (isPal madamimadam)" True (isPal_rec "madamimadam")
 assertEqual "for (isPal [1,2,4,8,16,8,4,2,1])" True (isPal_rec [1,2,4,8,16,8,4,2,1])

p06_test = do
 runTestTT p06_tests

p06_tests = TestList [
 TestLabel "test_isPal_rec_rec" test_isPal_rec
 ]
