-- Informatics 1 Functional Programming
-- December 2018
-- SITTING 2 (14:30 - 16:30)

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f str = sum [ b | (a,b) <- zip str [0..], isDigit a]

test1a = 
  f "" == 0 &&
  f "0 is the first position" == 0 &&
  f "I Love Functional Programming" == 0 &&
  f "2nite is 2 L8" == 21 &&
  f "0131 650 1000" == 66 &&
  f "1oTs & LoT5 of Num63r5" == 68
  

-- 1b

g :: String -> Int
g str = g' str 0
  where
    g' :: String -> Int -> Int
    g' [] n = 0
    g' (x:xs) n
        | isDigit x =  n + g' xs (n+1)
        | otherwise = g' xs (n+1)

test1b = 
  g "" == 0 &&
  g "0 is the first position" == 0 &&
  g "I Love Functional Programming" == 0 &&
  g "2nite is 2 L8" == 21 &&
  g "0131 650 1000" == 66 &&
  g "1oTs & LoT5 of Num63r5" == 68

prop1 :: String -> Bool
prop1 x = f x == g x 

-- Question 2

-- 2a

p :: [(Int,Int)] -> Bool
p str = sum [  a*b | (a,b) <- str, b >= 0] > 0

test2a =
  p [] == False &&
  p [(-1,-2),(-3,-5)] == False &&
  p [(4,5),(-7,3)] == False &&
  p [(4,5),(-6,3),(2,-2)] == True &&
  p [(4,5),(-6,3),(-2,2)] == False &&
  p [(4,-5),(-3,2),(1,6),(-3,-1)] == False

-- 2b

q :: [(Int,Int)] -> Bool 
q str = (q0 str)> 0 
  where 
    q0 :: [(Int,Int)] -> Int
    q0 [] = 0
    q0 ((a,b):xs)
        | b >= 0  = (a*b) + q0 xs
        | otherwise = q0 xs

test2b =
    q [] == False &&
    q [(-1,-2),(-3,-5)] == False &&
    q [(4,5),(-7,3)] == False &&
    q [(4,5),(-6,3),(2,-2)] == True &&
    q [(4,5),(-6,3),(-2,2)] == False &&
    q [(4,-5),(-3,2),(1,6),(-3,-1)] == False
  
-- 2c

r :: [(Int,Int)] -> Bool
r str = (foldr (+) 0 (map (\(x,y) -> x * y) (filter (\(x,y) -> y >= 0) str))) > 0

test2c =
  r [] == False &&
  r [(-1,-2),(-3,-5)] == False &&
  r [(4,5),(-7,3)] == False &&
  r [(4,5),(-6,3),(2,-2)] == True &&
  r [(4,5),(-6,3),(-2,2)] == False &&
  r [(4,-5),(-3,2),(1,6),(-3,-1)] == False

prop2 :: [(Int,Int)] -> Bool
prop2 x = p x == q x && q x == r x

-- Question 3

data Tree a = Lf a | Tree a :+: Tree a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized gen
    where
    gen 0 = liftM Lf arbitrary
    gen n | n>0 =
      oneof [liftM Lf arbitrary,
             liftM2 (:+:) tree tree]
      where
      tree = gen (n `div` 2)

-- 3a

left :: Tree a -> Bool
left = undefined

-- 3b

leaves :: Tree a -> [a]
leaves = undefined

-- 3c

shift :: Tree a -> Tree a
shift = undefined
