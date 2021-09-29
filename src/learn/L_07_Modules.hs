module L_07_Modules where

-- specific import
import Data.List (nub, tails, transpose)
--import qualified Data.Map
import qualified Data.Map as Map

uniqueList :: Eq a => [a] -> [a]
uniqueList = nub

mapFilter :: Ord k => [(k, a)] -> Map.Map k a
mapFilter = Map.fromList

matrixTranspose :: [[a]] -> [[a]]
matrixTranspose = transpose

lazyFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
lazyFoldr f z [] = z
lazyFoldr f z (x : xs) = f x $ lazyFoldr f z xs

strictFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
strictFoldr f z [] = z
strictFoldr f z (x : xs) = seq (f x z) strictFoldr f z xs

-- with short circuiting foldr
lookupr :: Eq a => a -> [a] -> Bool
lookupr x = lazyFoldr (\y inRest -> if x == y then True else inRest) False

flatMap :: [Integer]
flatMap = concatMap (replicate 4) [1 .. 3]

sumAllCubes :: Integer
sumAllCubes = sum $ takeWhile (< 10000) $ map (^ 3) [1 ..]

searchListAsSublist :: (Eq a) => [a] -> [a] -> Bool
searchListAsSublist needle haystack =
  let nlen = length needle
   in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

newtype Gen a = MkGen {unGen :: Int -> a}

x = MkGen {unGen = \x -> x + 1}

class Arbitrary a where
  arbitrary :: Gen a
  
instance Arbitrary Int where
  arbitrary = x

data MyType = MyType
  { foo :: Int,
    bar :: Bool,
    baz :: Float
  }
  deriving (Show)

x = MyType <$> arbitrary <*> arbitrary <*> arbitrary
