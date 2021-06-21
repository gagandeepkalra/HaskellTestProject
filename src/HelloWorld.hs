module HelloWorld
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello World!!!"


add :: Num a => a -> a -> a
add x y = x + y

a :: Integer -- Function0
a = 3

double :: Num a => a -> a
double x =  x + x

triple :: Integer -> Integer
triple x = double x + a

b :: Integer
b = if True then 5 else 6 + 1



