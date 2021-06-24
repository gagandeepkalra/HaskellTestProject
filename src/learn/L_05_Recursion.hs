module L_05_Recursion where

replicateN :: (Num i, Ord i) => i -> a -> [a]
replicateN i a
  | i <= 0 = []
  | otherwise = a : replicateN (i - 1) a

takeN :: (Num i, Ord i) => i -> [a] -> [a]
takeN 0 _ = []
takeN i (x : xs) = x : takeN (i -1) xs

repeatInfinity :: a -> [a]
repeatInfinity x = x : repeatInfinity x

zipL :: [a] -> [b] -> [(a, b)]
zipL _ [] = []
zipL [] _ = []
zipL (x : xs) (y : ys) = (x, y) : zipL xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let left = quicksort [xx | xx <- xs, xx <= x]
      right = quicksort [xx | xx <- xs, xx > x]
   in left ++ [x] ++ right
