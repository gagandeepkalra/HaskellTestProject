module L_06_HigherOrderFunctions where

applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)

zipListsWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipListsWith _ [] _ = []
zipListsWith _ _ [] = []
zipListsWith f (x : xs) (y : ys) = f x y : zipListsWith f xs ys

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList predicate (x : xs)
  | predicate x = x : filterList predicate xs
  | otherwise = filterList predicate xs

-- Collatz sequences. We take a natural number. If that number is even, we divide it by two. If it's odd, we multiply it
-- by 3 and then add 1 to that. We take the resulting number and apply the same thing to it.
-- It is thought that for all starting numbers, the chains finish at the number 1.
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

-- function composition
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]
