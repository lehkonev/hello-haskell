module Interactivity (input_asker, guesser, prompt) where

import System.IO
--import System.Random -- Won't work. Something broke when installing.

input_asker = do
  putStrLn ("Input asker:")
  hSetBuffering stdin LineBuffering
  user_input <- prompt ("Type something here: ")
  putStrLn ("You typed: \"" ++ user_input ++ "\"\n")

guesser = do
  putStrLn ("Number guesser:")
  hSetBuffering stdin LineBuffering
  num_str <- prompt ("Type a number between 1 and 100: ")
  let num = read (num_str) :: Int
  if (1 <= num) && (num <= 100)
    then do
      putStrLn ("Guess a number between 1 and 100. (Type 0 or less to stop guessing.)")
      do_guessing (num)
    else do
      putStrLn ("Invalid number. Stopping.")
  putStrLn ("")

do_guessing num = do
  user_input <- prompt ("Guess: ")
  let guess = read user_input :: Int
  if guess < 1
    then do
      putStrLn ("Stopped guessing.")
    else do
      if guess < num
        then do
          putStrLn ("Too low.")
          do_guessing (num)
        else if guess > num
          then do
            putStrLn ("Too high.")
            do_guessing (num)
          else do
            putStrLn ("You win!")

--------------------------------------------------------------------------------
-- Helper functions:

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine
