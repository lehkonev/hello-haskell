module Main where

-- Mine:
import SimpleBool
import Interactivity

-- Built-in:
import GHC.IO.Encoding
import System.IO
import Data.Typeable


data Triple a b c = Triple a b c deriving (Show)
data Quadruple a b = Quadruple a a b b deriving (Show)
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Show)
data MyList a = Nil | Constructor a (MyList a) deriving (Show, Read, Eq, Ord)
-- Or like this:
--data MyList a = Empty | Constructor { mylist_head :: a, mylist_tail :: MyList a } deriving (Show, Read, Eq, Ord)


main :: IO()
main = do
  setLocaleEncoding utf8 -- This is a must. I'm surprised åäö worked without.
  putStrLn "This is main.\n"

  input_asker

  word_list <- word_asker
  word_printer(word_list)

  guesser

  simplebool_xor
  calculations
  lists_of_pairs
  my_mapping
  lambdas

  file_reading

  typing
  datatypes
  maybe_stuff

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
calculations = do
  putStrLn ""
  putStrLn "Calculations:"

  putStrLn ("  Simple number constructor: " ++ show number1)

  number_list <- number_asker
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
my_mapping = do
  putStrLn ""
  putStrLn "My map:"
  let list1 = 3:6:2:7:8:4:[]
  putStrLn ("  Unsquared: " ++ show (list1))
  putStrLn ("  map:       " ++ show (map square list1))
  putStrLn ("  my_map:    " ++ show (my_map square list1))
  putStrLn "And then:"
  putStrLn ("  Map factorial: " ++ show (map factorial list1))
  putStrLn ("  Map fibonacci: " ++ show (map fibonacci list1))

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

datatypes = do
  putStrLn ""
  putStrLn "Datatypes:"

  putStrLn "  Triple datatype:"
  let tt = Triple 'a' 1 "eka"
  let t1 = triple_fst tt
  let t2 = triple_mid tt
  let t3 = triple_lst tt
  putStrLn("    Triple: " ++ show tt ++ "; triple_fst: " ++ show t1 ++ "; triple_mid: " ++ show t2 ++ "; triple_lst: " ++ show t3)

  putStrLn "  Quadruple datatype:"
  let qq = Quadruple 1 2 "eka" "toka"
  let q1 = quadruple_fst_two qq
  let q2 = quadruple_lst_two qq
  let t3 = triple_lst tt
  putStrLn("    Quadruple: " ++ show qq ++ "; quadruple_fst_two: " ++ show q1 ++ "; quadruple_lst_two: " ++ show q2)

  putStrLn "  Recursive datatype (MyList):"
  let ll = Constructor "blöp" Nil
  let ll1 = Constructor 4 (Constructor 2 (Constructor 8 (Constructor 5 Nil)))
  putStrLn ("    ll: " ++ show ll)
  putStrLn ("      length: " ++ show (mylist_length ll))
  putStrLn ("      head: " ++ show (mylist_head ll))
  putStrLn ("      tail: " ++ show (mylist_tail ll))
  putStrLn ("      last: " ++ show (mylist_last ll))
  putStrLn ("    ll1: " ++ show ll1)
  putStrLn ("      length: " ++ show (mylist_length ll1))
  putStrLn ("      head: " ++ show (mylist_head ll1))
  putStrLn ("      tail: " ++ show (mylist_tail ll1))
  putStrLn ("      last: " ++ show (mylist_last ll1))

maybe_stuff = do
  putStrLn ""
  putStrLn "Maybe stuff:"
  let some_list = [1,6,3]
  let empty_list = [] :: [Int] -- Has to have a stated type so that show knows what to do.
  putStrLn ("  some_list: " ++ show some_list ++ "; first element: " ++ show (first_ele some_list))
  putStrLn ("  empty_list: " ++ show empty_list ++ "; first element: " ++ show (first_ele empty_list))
  let t1 = Tuple1 "heyyy"
  let t2 = Tuple2 'w' 5
  let t3 = Tuple3 8 "aaa" ('a',"b")
  let t4 = Tuple4 4 8 3 5
  -- The commented parts cause an error:
  -- Ambiguous type variable `b0' arising from a use of `show'
  -- prevents the constraint `(Show b0)' from being solved.
  -- The problem only concerns the Nothings.
  --putStrLn ("Tuples 1, 2, 3, and 4: " ++ show t1 ++ "; " ++ show t2 ++ "; " ++ show t3 ++ "; " ++ show t4)
  putStrLn ("  tuple1: " ++ show (tuple1 t1) ++ "; " ++ show (tuple1 t2) ++ "; " ++ show (tuple1 t3) ++ "; " ++ show (tuple1 t4))
  putStrLn ("  tuple2: " {-++ show (tuple2 t1)-} ++ "; " ++ show (tuple2 t2) ++ "; " ++ show (tuple2 t3) ++ "; " ++ show (tuple2 t4))
  putStrLn ("  tuple3: " {-++ show (tuple3 t1) ++ "; " ++ show (tuple3 t2)-} ++ "; " ++ show (tuple3 t3) ++ "; " ++ show (tuple3 t4))
  putStrLn ("  tuple4: " {-++ show (tuple4 t1) ++ "; " ++ show (tuple4 t2) ++ "; " ++ show (tuple4 t3)-} ++ "; " ++ show (tuple4 t4))
  let what2 = tuple2 t1 -- This should be Nothing.
  if null what2
    then putStrLn ("  Second of t1 was null! " {-++ show what2-}) -- But how to print it?
    else putStrLn "  Second of t1 wasn't null?"

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


my_map f [] = []
my_map f (x:xs) =
  f x : my_map f xs

cf x = if x == 'a' then 'a' else 'x'

----

-- Triple functions:
triple_fst :: Triple a b c -> a
triple_fst (Triple x _ _) = x -- If some parameters are irrelevant, ignore them with _.
triple_mid :: Triple a b c -> b
triple_mid (Triple _ y _) = y
triple_lst :: Triple a b c -> c
triple_lst (Triple x y z) = z -- Can also be not ignored even if they do nothing.

-- Quadruple functions:
quadruple_fst_two :: Quadruple a b -> [a]
quadruple_fst_two (Quadruple x y _ _) = x:y:[]
quadruple_lst_two :: Quadruple a b -> [b]
quadruple_lst_two (Quadruple x y z n) = z:n:[]

--
first_ele :: [a] -> Maybe a
first_ele [] = Nothing
first_ele (x:xs) = Just x

--
tuple1 :: Tuple a b c d -> Maybe a
tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a _) = Just a
tuple1 (Tuple3 a _ _) = Just a
tuple1 (Tuple4 a _ _ _) = Just a

tuple2 :: Tuple a b c d -> Maybe b
tuple2 (Tuple1 a) = Nothing
tuple2 (Tuple2 a b) = Just b
tuple2 (Tuple3 a b c) = Just b
tuple2 (Tuple4 a b c d) = Just b

tuple3 :: Tuple a b c d -> Maybe c
tuple3 (Tuple1 a) = Nothing
tuple3 (Tuple2 a b) = Nothing
tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just c

tuple4 :: Tuple a b c d -> Maybe d
tuple4 (Tuple1 a) = Nothing
tuple4 (Tuple2 a b) = Nothing
tuple4 (Tuple3 a b c) = Nothing
tuple4 (Tuple4 a b c d) = Just d

mylist_length :: MyList a -> Integer
mylist_length Nil = 0
mylist_length (Constructor x xs) = 1 + mylist_length xs

mylist_head :: MyList a -> a
--mylist_head Nil = Nil -- Doesn't work; how to exception?
mylist_head (Constructor x xs) = x

mylist_tail :: MyList a -> MyList a
mylist_tail (Constructor x xs) = xs

mylist_last :: MyList a -> a
mylist_last (Constructor x Nil) = x
mylist_last (Constructor x xs) = mylist_last xs
