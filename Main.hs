module Main where

import SimpleBool

main = do
  putStrLn "This is main."
  let b1 = True
  let b2 = False
  putStrLn("Bool true: " ++ show b1 ++ "; bool false: " ++ show b2)
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
  
  print number2



number1 :: Num a => a
number1 = 1 + 5 + 7 + 3 + 2

number2 :: Num a => a
number2 = number1 * number1



{-
import Hello

main = do
  putStrLn "This is main."
  hello
-}
