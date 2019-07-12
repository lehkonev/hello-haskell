module Main where

-- Mine:
import SimpleBool
import Interactivity
import Datatypes

-- Built-in:
import GHC.IO.Encoding
import System.IO
import Data.Typeable
import Data.Char


main :: IO()
main = do
  setLocaleEncoding utf8 -- This is a must. I'm surprised åäö worked without.
  putStrLn "This is main."

  input_asker

  word_list <- word_asker -- word_list :: [String]
  --let word_list = word_asker -- word_list :: IO [String]
  word_printer(word_list)

  guesser

  simplebool_xor

  number_list <- number_asker
  calculations number_list
  listing number_list

  lists_of_pairs
  my_mapping number_list
  lambdas

  file_reading

  typing
  maybe_stuff
  datatypes number_list

  arraying

  monad_test

  putStrLn ""
  putStrLn "End of program."

----

simplebool_xor = do
  putStrLn ""
  putStrLn "SimpleBool xor:"
  let b1 = True
  let b2 = False
  putStrLn("  Bool true: \"" ++ show b1 ++ "\"; bool false: \"" ++ show b2 ++ "\"")
  putStrLn("  " ++ show b1 ++ " xor " ++ show b1 ++ " = " ++ show (xorbool b1 b1))
  putStrLn("  " ++ show b1 ++ " xor " ++ show b2 ++ " = " ++ show (xorbool b1 b2))
  putStrLn("  " ++ show b2 ++ " xor " ++ show b1 ++ " = " ++ show (xorbool b2 b1))
  putStrLn("  " ++ show b2 ++ " xor " ++ show b2 ++ " = " ++ show (xorbool b2 b2))

  let n1 = 4
  let n2 = 6
  putStrLn("  " ++ show n1 ++ " xor " ++ show n1 ++ " = " ++ show (xorbool n1 n1))
  putStrLn("  " ++ show n1 ++ " xor " ++ show n2 ++ " = " ++ show (xorbool n1 n2))

  let s1 = "aa"
  let s2 = "bee"
  putStrLn("  " ++ s1 ++ " xor " ++ s1 ++ " = " ++ show (xorbool s1 s1))
  putStrLn("  " ++ s1 ++ " xor " ++ s2 ++ " = " ++ show (xorbool s1 s2))

--
calculations number_list = do
  putStrLn ""
  putStrLn "Calculations:"

  putStrLn ("  Simple number constructor: " ++ show number1)

  if number_list == []
    then putStrLn "  There are no numbers."
    else do
      putStr ("  List of numbers: ")
      print (number_list)
      putStrLn ("  Filter the rest according to first (smaller than): " ++ show (filter (<(head number_list)) (tail number_list)))
      let number_sum = foldr (+) 0 number_list
      putStrLn ("  Sum of numbers: " ++ show (number_sum))
      let number_sum = foldr (-) 0 number_list
      putStrLn ("  Difference-r of numbers: " ++ show (number_sum) ++ " -- if [a,b,c] then (a - (b - (c - 0)))")
      let number_sum = foldl (-) 0 number_list
      putStrLn ("  Difference-l of numbers: " ++ show (number_sum) ++ " -- if [a,b,c] then (((0 - a) - b) - c)")
      let number_product = foldr (*) 1 number_list
      putStrLn ("  Product of numbers: " ++ show (number_product))
      let number_mult = foldr (mult) 1 number_list
      putStrLn ("  Product of numbers: " ++ show (number_mult) ++ " (using only summing)")
      number_operation_series "  Factorial" factorial number_list
      number_operation_series "  Fibonacci" fibonacci number_list
      number_operation_series "  Square" square number_list

  let abc = [1,2,0] :: [Float]
  putStrLn ("  For a list of coefficients " ++ show abc ++ ", the roots are " ++ show (roots abc) ++ ".")

--
listing numbers = do
  putStrLn ""
  putStrLn "List action:"
  if null numbers
    then putStrLn "  Zipping: No numbers to zip."
    else do
      let len = length numbers
      let ns = take len [-4,-8..]
      let alist = zip numbers ns
      putStrLn ("  Zipping: " ++ show alist)

--
lists_of_pairs = do
  putStrLn ""
  putStrLn "Lists of pairs:"

  let list_of_pairs = [(5,'b'),(1,'c'),(6,'a')]
  let list_result1 = fst (head (tail (list_of_pairs)))
  let str_result1 = show list_of_pairs ++ " -> " ++ show list_result1
  putStrLn ("  " ++ str_result1)

  putStrLn ("  Same: " ++ show (first_of_second_of_list (list_of_pairs)))
  putStrLn ("  Same: " ++ show (fst (head (tail (list_of_pairs)))))
  putStrLn ("  Same: " ++ show (first_of_second_of_list (list_of_pairs)))

--
my_mapping number_list = do
  if null number_list
    then my_mapping (3:6:2:7:8:4:[])
    else do
      putStrLn ""
      putStrLn "My map:"
      putStrLn ("  Unsquared: " ++ show number_list)
      putStrLn ("  map:       " ++ show (map square number_list))
      putStrLn ("  my_map:    " ++ show (my_map square number_list))
      putStrLn "And then:"
      putStrLn ("  Map factorial: " ++ show (map factorial number_list))
      putStrLn ("  Map fibonacci: " ++ show (map fibonacci number_list))

--
lambdas = do
  putStrLn "\nLambdas:"

  -- λx.x*x
  --   λx. : take value x
  --   x*x : multiply x with itself

  let sqr = \x -> x*x
  putStrLn "  λx.x*x"
  putStrLn "    sqr = \\x -> x*x"
  putStrLn ("    sqr 5 = " ++ show (sqr 5))

  let some_f = \x y -> 2*x + y
  putStrLn "  λxλy.2*x+y"
  putStrLn "    some_f = \\x y -> 2*x + y"
  putStrLn ("    some_f 5 4 = " ++ show (some_f 5 4))

--
file_reading = do
  putStrLn "\nFile-reading:"
  -- Note: the file needs to be utf8-encoded. And it still doesn't print åäö well.
  file_content <- readFile "file1.txt"
  putStrLn "  Contents of the file:"
  print file_content
  putStrLn "  End of file."
  putStrLn "  Contents of the file in CAPITALS:"
  print (map Data.Char.toUpper file_content)
  putStrLn "  End of file."
  putStrLn "Here are some special characters, just in case: åäö λµ– 日本語"

--
typing = do
  putStrLn ""
  putStrLn "Typing:"
  let f1 = \x -> [x]
  let f2 = \x y z -> (x,y:z:[])
  let f3 = \x -> x + 5
  let f4 = \x -> "hello, world"
  let f5 = \x -> x 'a'
  let f6 = \x -> x 'a'
  --let f6 = \x -> x x
  let f7 = \x -> x + x

  -- Doesn't work:
  --putStrLn("  -- Type of f1: " ++ (show (typeOf f1)) ++ " --")

  -- GHCI gives the types, though:
  putStrLn "  Expression and type given by GHCI                        Parameter  Result"
  putStrLn ("  (\\x -> [x])            :: a -> [a]                      f1 4     = " ++ show (f1 4))
  putStrLn ("  (\\x y z -> (x,y:z:[])) :: a1 -> a2 -> a2 -> (a1, [a2])  f2 4 2 9 = " ++ show (f2 4 2 9))
  putStrLn ("  (\\x -> x + 5)          :: Num a => a -> a               f3 4     = " ++ show (f3 4))
  putStrLn ("  (\\x -> \"hello, world\") :: p -> [Char]                   f4 4     = " ++ show (f4 4))
  putStrLn ("  (\\x -> x 'a')          :: (Char -> t) -> t              f5 cf    = " ++ show (f5 cf))
  putStrLn ("  (\\x -> x x)                                             f6         nope (infinite type error)")
  putStrLn ("  (\\x -> x + x)          :: Num a => a -> a               f7 4     = " ++ show (f7 4))
  putStrLn "  Note:"
  putStrLn "    f4: The parameter is just discarded, whatever it is."
  putStrLn "    f5: Works if the parameter is any function that takes a character as a parameter."

--
monad_test = do
  putStrLn ""
  putStrLn "Monad test:"
  putStr "  Type something beginning with asd: "
  hFlush stdout
  ('a':'s':'d':x:xs) <- getLine
  putStr "  Removed the asd: "
  putStrLn (x:xs)

------------------------------------------------------------------------------
-- Helper functions:

-- Number functions: ------------------

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
-- factorial limits its parameter to an Integral. It takes a value of that
-- type as a parameter and returns a value of that same type.
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

--mult :: (Integral a) => a -> a
mult a 0 = 0
mult a 1 = a
mult a b =
    if b < 0
        then 0 - mult a (-b)
        else a + mult a (b-1)

-- This function performs the given operation to all numbers in number_list.
-- The functions (this far) that could be in the operation variable work with
-- non-Integers (like Ints), but I assume since the numbers number_list are
-- limited to Integers, the type of the function needs to be limited to
-- Integers too.
number_operation_series :: String -> (Integer -> Integer) -> [Integer] -> IO ()
number_operation_series name operation [] = return ()
number_operation_series name operation (number:number_list) = do
  let result = operation number
  putStrLn (name ++ " of " ++ show number ++ ": " ++ show result)
  number_operation_series name operation number_list

roots :: [Float] -> (Float, Float)
roots abc =
  let a = head abc
      b = head (tail abc)
      c = head (tail (tail abc))
      discriminant = sqrt (b*b - 4*a*c)
      twice_a = 2*a
  in ((-b + discriminant) / twice_a,
      (-b - discriminant) / twice_a)

-- Other functions: -------------------

first_of_second_of_list :: [(a,b)] -> a
first_of_second_of_list list = fst (head (tail (list)))

--
my_map f [] = []
my_map f (x:xs) = f x : my_map f xs

cf x = if x == 'a' then 'a' else 'x'
