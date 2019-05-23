module Interactivity (input_asker, guesser, prompt) where

import System.IO

input_asker = do
  hSetBuffering stdin LineBuffering
  user_input <- prompt ("Input: ")
  putStrLn ("You typed: \"" ++ user_input ++ "\"\n")  

guesser = do
  putStrLn ("guesser not implemented yet.")

--------------------------------------------------------------------------------
-- Helper functions:

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine
