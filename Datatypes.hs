module Datatypes (
  maybe_stuff,
  datatypes,
  arraying
  )
    where

import Interactivity

import GHC.IO.Encoding
import System.IO
import Data.Typeable
import Data.Array
import qualified Data.Map as Map


data Triple a b c = Triple a b c deriving (Show)
data Quadruple a b = Quadruple a a b b deriving (Show)
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Show)
data MyList a = Nil | Constructor a (MyList a) deriving (Show, Read, Eq, Ord)
-- Or like this:
--data MyList a = Empty | Constructor { mylist_head :: a, mylist_tail :: MyList a } deriving (Show, Read, Eq, Ord)
-- Why doesn't this tree work if node is in the middle?
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)

-- A datatype with named fields for easy access and updating:
data PathfinderCharacter =
  PathfinderCharacter { cname  :: String,
                        crace  :: String,
                        cclass :: String,
                        clevel :: Integer
                      }
                      deriving (Read, Eq)

instance Show PathfinderCharacter where
  show c = cname c ++ ", " ++ crace c ++ " " ++ cclass c ++ " " ++ show (clevel c)


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

  putStrLn "  Maybe Integer:"
  int_from_prompt <- prompt_int_m "    Give an integer again: "
  putStrLn ("    -- Type of int_from_prompt: " ++ (show (typeOf int_from_prompt)) ++ " --")
  putStrLn ("    Value of int_from_prompt: " ++ show int_from_prompt)

datatypes :: [Integer] -> IO ()
datatypes test_numbers = do
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
  let ll2 = mylist_create (if null test_numbers then [4,3,-9,2,7,1] else test_numbers)
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
  putStrLn ("    ll2: " ++ show ll2)
  putStrLn ("      length: " ++ show (mylist_length ll2))
  putStrLn ("      head: " ++ show (mylist_head ll2))
  putStrLn ("      tail: " ++ show (mylist_tail ll2))
  putStrLn ("      last: " ++ show (mylist_last ll2))

  putStrLn "  Recursive datatype (BinaryTree):"
  let bt1 = singleton_tree 3
  let bt2 = tree_insert 4 bt1
  let bt3 = tree_insert 1 bt2
  let bt4 = tree_insert (-4) bt3
  let bt5 = tree_insert 6 bt4
  let bt6 = tree_insert 32 bt5
  putStrLn ("    bt1 (" ++ show (tree_size bt1) ++ "): " ++ show bt1)
  putStrLn ("    bt2 (" ++ show (tree_size bt2) ++ "): " ++ show bt2)
  putStrLn ("    bt3 (" ++ show (tree_size bt3) ++ "): " ++ show bt3)
  putStrLn ("           : " ++ tree_print bt3)
  putStrLn ("    bt4 (" ++ show (tree_size bt4) ++ "): " ++ tree_print bt4)
  putStrLn ("    bt5 (" ++ show (tree_size bt5) ++ "): " ++ tree_print bt5)
  putStrLn ("    bt6 (" ++ show (tree_size bt6) ++ "): " ++ tree_print bt6)
  putStrLn ("    bt6 elements: " ++ show (tree_elements bt6))
  let bt = tree_create (if null test_numbers then [4,3,-9,2,7,6] else test_numbers)
  putStrLn ("    bt (" ++ show (tree_size bt) ++ "): " ++ tree_print bt)
  let btf = fmap (+2) bt
  putStrLn ("    btf (" ++ show (tree_size bt) ++ "): " ++ tree_print btf)
  putStrLn ("    btf (" ++ show (tree_size EmptyTree) ++ "): " ++ tree_print (fmap (^2) EmptyTree))

  putStrLn "  Exciting Pathfinder character datatype:"
  characters <- ask_user_for_characters
  putStrLn ("    -- Type of characters: " ++ (show (typeOf characters)) ++ " --")
  if null characters
    then putStrLn "    No characters."
    else do
      let len = fromIntegral (length characters) :: Integer
      let indices = test_numbers ++ [1..len] -- In case there are less test_numbers than characters.
      let character_map = Map.fromList (zip indices characters)
      putStrLn ("    List of " ++ show len ++ " characters:")
      putStrLn (unlines (map (("      " ++) . show) characters))
      -- How to not print the empty list item?
      let i = head indices
      putStrLn ("    Character " ++ show i ++ ":")
      putStrLn ("      " ++ show (Map.lookup i character_map))
      putStrLn ("    Map of " ++ show len ++ " characters (in a long string because that's what we can get):")
      putStrLn ("      " ++ show character_map)

arraying = do
  putStrLn ""
  putStrLn "Arrays:"
  -- If the bounds (1,5) differ from the real size of the list(s), the
  -- following error occurs:
  --main.exe: (Array.!): undefined array element
  -- With             list comprehensions
  let a4 = array (1,4) [(i,i^2) | i <- [1..4]]
  putStrLn ("  " ++ show a4)
  let a1 = array (1,5) [(i,j) | i <- [1..5], j <- [11..15]]
  putStrLn ("  " ++ show a1)
  let a2 = listArray (1,10) [1..10]
  putStrLn ("  " ++ show a2)
  let a3 = accumArray (+) 2 (1,5) [(i,i) | i <- [1..5]]
  putStrLn ("  " ++ show a3)

----

ask_user_for_characters :: IO [PathfinderCharacter]
ask_user_for_characters = do
  putStrLn "    Give info for a new character (empty to stop):"
  hSetBuffering stdin LineBuffering

  iname <- prompt "      Character name: "
  if null iname
    then do
      putStrLn "    Stopped asking for character info."
      return ([] :: [PathfinderCharacter])

    else do
      irace <- prompt "      Character race: "
      if null irace
        then do
          putStrLn "    Stopped asking for character info."
          return ([] :: [PathfinderCharacter])

        else do
          iclass <- prompt "      Character class: "
          if null iclass
            then do
              putStrLn "    Stopped asking for character info."
              return ([] :: [PathfinderCharacter])

            else do
              ilevel <- prompt_int "      Character level: "
              if ilevel < 1
                then do
                  putStrLn "    Stopped asking for character info."
                  return ([] :: [PathfinderCharacter])

                else do
                  let c = PathfinderCharacter {cname=iname, crace=irace, cclass=iclass, clevel=ilevel}
                  rest <- ask_user_for_characters
                  return (c:rest)

----

-- Tuple functions:
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

-- MyList functions:
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

mylist_create :: [a] -> MyList a
mylist_create (x:[]) =
  Constructor x Nil
mylist_create (x:xs) =
  Constructor x (mylist_create xs)

-- Binary tree functions:
singleton_tree :: a -> Tree a
singleton_tree x = (Node x) EmptyTree EmptyTree

tree_insert :: (Ord a) => a -> Tree a -> Tree a
tree_insert x EmptyTree = singleton_tree x
tree_insert x (Node n left right)
  | x == n = Node x left right
  | x < n  = Node n (tree_insert x left) right
  | x > n  = Node n left (tree_insert x right)

tree_size :: Tree a -> Integer
tree_size EmptyTree = 0
tree_size (Node n left right) =
  1 + tree_size left + tree_size right

tree_elements :: Tree a -> [a]
tree_elements EmptyTree = []
tree_elements (Node n left right) =
  (tree_elements left) ++ [n] ++ (tree_elements right)

tree_create :: (Ord a) => [a] -> Tree a
tree_create [] = EmptyTree
tree_create (x:xs) =
  tree_insert x (tree_create xs)

tree_print :: (Show a) => Tree a -> String
tree_print EmptyTree = "E"
tree_print (Node n left right) =
  "(" ++ show n ++ " " ++ tree_print left ++ " " ++ tree_print right ++ ") "
