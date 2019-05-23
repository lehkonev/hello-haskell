module Interactivity (input_asker, guesser) where

import System.IO

input_asker = do
  hSetBuffering stdin LineBuffering
  putStrLn "Input: "
  user_input <- getLine
  putStrLn ("You typed: \"" ++ user_input ++ "\"")  

guesser = do
  putStrLn ("guesser not implemented yet.")