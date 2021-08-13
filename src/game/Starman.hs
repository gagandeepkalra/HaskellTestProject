module Starman where

check :: String -> String -> Char -> (Bool, String)
check secret display c =
  (c `elem` secret, [if x == c then c else y | (x, y) <- zip secret display])

mkGuess :: String -> String -> Int -> IO ()
mkGuess secret display n =
  do
    putStr "Enter your guess- "
    c <- getLine
    let (correct, display') = check secret display $ head c
    let n' = (if correct then n else n -1)
    putStrLn $ "turns remaining " ++ show n'
    putStrLn display'
    turn secret display' n'

turn :: String -> String -> Int -> IO ()
turn secret display n =
  do
    if n == 0
      then putStrLn "You Lose!"
      else
        if secret == display
          then putStrLn "You win!"
          else mkGuess secret display n

-- Starman. In this single-player, text-based game, there is a word which the player needs to guess. For each turn of the game,
-- the player guesses a single letter. If that letter is correct, then the guessed letters are displayed in the correct places in the word.
-- If that letter is incorrect, then the user loses a star. Once the user has no stars left, they have lost the game.
-- However if the user guesses all the letters in the word, they have won the game.
starman :: String -> Int -> IO ()
starman secret = turn secret ['_' | _ <- secret]
