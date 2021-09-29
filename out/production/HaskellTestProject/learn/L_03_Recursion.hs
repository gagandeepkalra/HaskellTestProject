module L_03_Recursion where

replicateTimes :: (Num i, Ord i) => i -> a -> [a]
replicateTimes i a
  | i == 0 = []
  | otherwise = a : replicateTimes (i -1) a
