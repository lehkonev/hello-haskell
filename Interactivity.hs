module Interactivity (
  input_asker,
  word_asker,
  word_printer,
  guesser,
  prompt)
    where

import Data.Typeable
import System.IO
--import System.Random -- Won't work. Something broke when installing.

-- input_asker is an IO action that has nothing () to give out.
input_asker :: IO ()
input_asker = do
  putStrLn ("Input asker:")
  hSetBuffering stdin LineBuffering
  user_input <- prompt ("Type something here: ")
  putStrLn("Type of user_input: " ++ (show (typeOf user_input)))
  putStrLn ("You typed: \"" ++ user_input ++ "\"\n")

-- word_asker is an IO action whose result will be a list of Strings, so its
-- type is IO [String]. The result is extracted in main
-- (word_list <- word_asker), and word_list is a [String].
word_asker :: IO [String]
word_asker = do
  putStrLn "Repeating word asker (press only enter to stop):"
  putStrLn("Type of word_asker: " ++ (show (typeOf word_asker)))
  ask_for_words

ask_for_words :: IO [String]
ask_for_words = do
  hSetBuffering stdin LineBuffering
  user_input <- prompt ("Please enter a word: ")
  if user_input == ""
    then do
      putStrLn ("Quit asking words.")
      return []
    else do
      rest <- ask_for_words
      return (user_input : rest)

word_printer :: [String] -> IO ()
word_printer word_list = do
  putStrLn("Word printer:")
  putStrLn("Type of word_list: " ++ (show (typeOf word_list)) ++ " (not IO)")
  putStrLn("Word list concatenated into a long string:")
  let char_list = foldr (++) "" word_list
  putStrLn(char_list)
  putStrLn("Word list, newline-separated:")
  let char_list2 = unlines word_list
  putStrLn(char_list2)

guesser :: IO ()
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

do_guessing :: Int -> IO ()
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
