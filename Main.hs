module Main where

import SimpleBool
import Interactivity

main = do
  putStrLn "This is main.\n"

  input_asker

  word_list <- word_asker
  word_printer(word_list)

  guesser

  simplebool_xor
  calculations
  lists_of_pairs
  my_mapping

  putStrLn "\nEnd of program."

----

simplebool_xor = do
  putStrLn "SimpleBool xor:"
  let b1 = True
  let b2 = False
  putStrLn("Bool true: \"" ++ show b1 ++ "\"; bool false: \"" ++ show b2 ++ "\"")
  putStrLn(show b1 ++ " xor " ++ show b1 ++ " = " ++ show (xorbool b1 b1))
  putStrLn(show b1 ++ " xor " ++ show b2 ++ " = " ++ show (xorbool b1 b2))
  putStrLn(show b2 ++ " xor " ++ show b1 ++ " = " ++ show (xorbool b2 b1))
  putStrLn(show b2 ++ " xor " ++ show b2 ++ " = " ++ show (xorbool b2 b2))

  let n1 = 4
  let n2 = 6
  putStrLn(show n1 ++ " xor " ++ show n1 ++ " = " ++ show (xorbool n1 n1))
  putStrLn(show n1 ++ " xor " ++ show n2 ++ " = " ++ show (xorbool n1 n2))

  let s1 = "aa"
  let s2 = "bee"
  putStrLn(s1 ++ " xor " ++ s1 ++ " = " ++ show (xorbool s1 s1))
  putStrLn(s1 ++ " xor " ++ s2 ++ " = " ++ show (xorbool s1 s2))

--
calculations = do
  putStrLn "\nCalculations:"

  print number1
  putStrLn ("Square of " ++ show number1 ++ ": " ++ show (square number1))
  putStrLn ("Square of 5: " ++ show (square 5))

  putStrLn ("Factorial of 5: " ++ show (factorial 5))
  putStrLn ("Factorial of 7: " ++ show (factorial 7))

  putStrLn ("Fibonacci sequences of 1 to 9:")
  putStrLn (show (fibonacci(1)))
  print (fibonacci(2))
  print (fibonacci(3))
  print (fibonacci(4))
  print (fibonacci(5))
  print (fibonacci(6))
  print (fibonacci(7))
  print (fibonacci(8))
  print (fibonacci(9))

  putStrLn ("Multiplication, using only addition:")
  putStrLn ("0*67=" ++ show (mult 0 67) ++ "; 45*0=" ++ show (mult 45 0) ++ "; 5*6=" ++ show (mult 5 6))

--
lists_of_pairs = do
  putStrLn "\nLists of pairs:"

  let list_of_pairs = [(5,'b'),(1,'c'),(6,'a')]
  let list_result1 = fst (head (tail (list_of_pairs)))
  let str_result1 = show list_of_pairs ++ " -> " ++ show list_result1
  putStrLn (str_result1)

  putStrLn ("Same: " ++ show (first_of_second_of_list (list_of_pairs)))
  putStrLn ("Same: " ++ show (fst (head (tail (list_of_pairs)))))
  putStrLn ("Same: " ++ show (first_of_second_of_list (list_of_pairs)))

--
my_mapping = do
  putStrLn "\nMy map:"
  let list1 = 3:6:2:7:8:4:[]
  putStrLn ("Square: " ++ show (list1))
  putStrLn ("map:    " ++ show (map square list1))
  putStrLn ("my_map: " ++ show (my_map square list1))

-- Helper functions:

-- Construct a number.
number1 :: Num a => a
number1 = 1 + 5 + 7 + 3 + 2

-- Square a number.
-- "Num a => a" restricts the type of the parameter. "Num a" on the left is
-- the constraint and "a" on the right is the actual type.
square :: Num a => a -> a
square x = (*) x x --non-infix
-- square x = x * x --infix

-- Return -1 if x < 0, return 1 if x > 0.
-- The "Ord" typeclass means that values of that type can be ordered.
signum :: (Num a, Ord a) => a -> a
signum x =
  if x < 0
    then -1
    else if x > 0
      then 1
      else 0

-- Factorial function, recursive.
factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial n = n * factorial (n-1)

-- Fibonacci function.
fibonacci :: (Integral a) => a -> a
fibonacci n =
  if n <= 0 then
    0
  else
    case n of
      1 -> 1
      2 -> 1
      _ -> fibonacci (n-2) + fibonacci (n-1)

-- Multiplication only using addition.
{-
mult a b =
  case a of
    0 -> 0
    1 -> b
    _ -> b + mult (a-1 b)
-}

--mult :: (Integral a) => a -> a
mult a 0 = 0
mult a 1 = a
mult a b =
    if b < 0
        then 0 - mult a (-b)
        else a + mult a (b-1)

--first_of_second_of_list :: [a] -> b -- How to type?
first_of_second_of_list list = fst (head (tail (list)))


my_map f [] = []
my_map f (x:xs) =
  f x : my_map f xs

{-
import Hello

main = do
  putStrLn "This is main."
  hello
-}
