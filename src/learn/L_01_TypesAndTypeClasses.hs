module L_01_TypesAndTypeClasses where

equal :: Eq a => a -> a -> Bool
equal a b = a == b

f :: a -> a
f x = x

g :: a -> a
g x = x

greaterThan :: Ord a => a -> a -> Bool
greaterThan a b = a > b

wellRead :: [Integer]
wellRead = read "[1,2,3,4]" ++ [3]

a :: Int
a = read "1" :: Int

convertFromIntegral :: (Integral a, Num b) => a -> b
convertFromIntegral = fromIntegral

addToLength :: Foldable t => t a -> Double
addToLength ls = (convertFromIntegral . length) ls + 3.2
