module L_04_SyntaxInFunctions where

-- pattern matching
factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- non exhaustive
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- function pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

listLength :: (Num b) => [a] -> b
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs

-- let expression
squarePair :: (Num a, Num b) => (a, b) -> (a, b)
squarePair (x, y) = let square a = a * a in (square x, square y)

-- case expression
listHead :: [a] -> a
listHead xs = case xs of
  [] -> error "No head for empty lists!"
  (x : _) -> x

-- where binding
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
