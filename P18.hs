module P18 (p18_test) where

import Test.HUnit
import Data.List

slice_rec :: [a] -> Int -> Int -> [a]
slice_rec [] j k = []
slice_rec (l:ls) 0 0 = [l]
slice_rec (l:ls) 0 k = [l] ++ slice_rec ls 0 (k - 1)
slice_rec (l:ls) j k = [] ++ (slice_rec ls (j - 1) (k - 1))
{-
slice_rec (l:ls) j k = if j > 0 then slice_rec ls (j - 1) (k - 1) else ([l] ++ slice_rec ls 0 (k - 1))
-}


slice_rec_2 :: [a] -> Int -> Int -> [a]
slice_rec_2 [] j k = []
slice_rec_2 (l:ls) 1 1 = [l]
slice_rec_2 (l:ls) 1 k = [l] ++ slice_rec_2 ls 1 (k - 1)
slice_rec_2 (l:ls) j k = [] ++ (slice_rec_2 ls (j - 1) (k - 1))


slice_rec_3 l j k = [ l !! (i-1) | i <- [1..(length l - 1)], i >= j, i <= k ]


slice_rec_4 l j k = [ l !! (i-1) | i <- [j..k] ]

{- BAD -}
slice_map l j k = case owned of (a,b) -> b
	where owned = (mapAccumL (\acc x -> (acc,x)) j l)
{-
slice_map l j k = case owned of (a,[b]) -> b
	where owned = (mapAccumL (\acc x -> if fst acc >= j then (((fst acc)+1,x),x) else (((fst acc)+1,x),x)) (0,[]) l)
-}
{-	where owned = (mapAccumL (\acc x -> (acc+1, l !! acc)) j l) -}


{- FOLD -}
{-slice_fold l j k = foldl (\a b -> if b >= j then (a ++ [b]) else (a)) [] l -}

-- e
slice_fold l j k = snd . foldr (\b (n,a) -> if n >= j && n <= k then (n+1,[b] ++ a) else (n+1,a)) (0, []) $ l

slice_fold_2 l j k = snd . foldr (
								\b (n, a) ->
									if n >= j && n <= k then
										(n+1, [b] ++ a)
									else
										(n+1,a))

								(0, []) $ l



-- Tests

test_slice_rec_2 = TestCase $ do
	assertEqual "for (slice_rec_2 'abcdefghik' 3 7" "cdefg" (slice_rec_2 "abcdefghik" 3 7)

test_slice_rec_3 = TestCase $ do
	assertEqual "for (slice_rec_3 'abcdefghik' 3 7" "cdefg" (slice_rec_3 "abcdefghik" 3 7)


test_slice_rec_4 = TestCase $ do
	assertEqual "for (slice_rec_4 'abcdefghik' 3 7" "cdefg" (slice_rec_4 "abcdefghik" 3 7)

test_slice_map = TestCase $ do
	assertEqual "for (slice_map 'abcdefghik' 3 7)" "cdefg" (slice_map "abcdefghik" 3 7)

test_slice_fold = TestCase $ do
	assertEqual "for (slice_fold 'abcdefghik' 3 7)" "cdefg" (slice_fold "abcdefghik" 3 7)

p18_test = do
	runTestTT p18_tests


p18_tests = TestList [
				TestLabel "test_slice_rec_2" test_slice_rec_2,
				TestLabel "test_slice_rec_3" test_slice_rec_3,
				TestLabel "test_slice_rec_3" test_slice_rec_4,
				TestLabel "test_slice_map" test_slice_map,
				TestLabel "test_slice_fold" test_slice_fold
			]
