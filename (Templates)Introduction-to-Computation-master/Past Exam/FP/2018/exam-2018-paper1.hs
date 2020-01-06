-- Informatics 1 Functional Programming
-- December 2018
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f x = sum [ b | (a,b) <- zip x [0..], isUpper a ]

test1a =
  f "" == 0 &&
  f "no capitals here" == 0 &&
  f "Positions start from 0" == 0 &&
  f "ALL CAPS" == 25 &&
  f "I Love Functional Programming" == 27 &&
  f "1oTs & LoT5 of Num63r5" == 33

-- 1b
-- construct helper functions 
g :: String -> Int
g str = g' str 0
  where
    g' [] n = 0
    g' (x:xs) n 
      | isUpper x = n + (g' xs (n+1))
      | otherwise = g' xs (n+1)

test1b =
    g "" == 0 &&
    g "no capitals here" == 0 &&
    g "Positions start from 0" == 0 &&
    g "ALL CAPS" == 25 &&
    g "I Love Functional Programming" == 27 &&
    g "1oTs & LoT5 of Num63r5" == 33

prop1 :: String -> Bool
prop1 x = f x == g x    

-- Question 2

-- 2a

p :: [(Int,Int)] -> Bool
p str = sum [x^2| (x,_) <- str] > product [ y| (_,y) <- str, odd y]

test2a = 
  p [] == False &&
  p [(4,5),(1,3)] == True &&
  p [(4,5),(1,2),(2,7)] == False &&
  p [(-1,3),(1,1)] == False &&
  p [(1,2),(2,3),(3,5)] == False && 
  p [(2,2),(2,3),(3,5)] == True

-- 2b

q :: [(Int,Int)] -> Bool
q str = q1 str > q2 str
  where
    q1 [] = 0
    q1 ((a,b):xs) = a^2 + q1 xs
    q2 [] = 1
    q2 ((a,b):xs) | odd b = b * q2 xs
                  | otherwise = q2 xs 
  
test2b = 
  q [] == False &&
  q [(4,5),(1,3)] == True &&
  q [(4,5),(1,2),(2,7)] == False &&
  q [(-1,3),(1,1)] == False &&
  q [(1,2),(2,3),(3,5)] == False && 
  q [(2,2),(2,3),(3,5)] == True

-- 2c

r :: [(Int,Int)] -> Bool
r str = (foldr (+) 0 (map ((^2).fst) str)) > 
        (foldr (*) 1 (map snd (filter (\(x,y) -> odd y) str)))

test2c = 
  r [] == False &&
  r [(4,5),(1,3)] == True &&
  r [(4,5),(1,2),(2,7)] == False &&
  r [(-1,3),(1,1)] == False &&
  r [(1,2),(2,3),(3,5)] == False && 
  r [(2,2),(2,3),(3,5)] == True

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

right :: Tree a -> Bool
right (Lf a) = True
right (Lf a :+: b ) = right b
right ( _ :+: _) = False



test3a = 
  right (Lf 1) ==  True &&
  right(Lf 1:+:(Lf 2:+:(Lf 3:+:Lf 4))) ==  True && 
  right((Lf 1:+:Lf 2):+:(Lf 3:+:Lf 4)) ==  False &&
  right (Lf "a" :+: (Lf "b" :+: Lf "c")) ==  True && 
  right ((Lf "a" :+: Lf "b") :+: Lf "c") ==  False


-- 3b

leaves :: Tree a -> [a]
leaves (Lf a)= [a]
leaves (a :+: b) = leaves a ++ leaves b

test3b = 
  leaves (Lf 1)                                 ==  [1] &&
  leaves (Lf 1 :+: (Lf 2 :+: (Lf 3 :+: Lf 4)))  ==  [1,2,3,4] &&
  leaves ((Lf 1 :+: Lf 2) :+: (Lf 3 :+: Lf 4))  ==  [1,2,3,4] &&
  leaves (Lf "a" :+: (Lf "b" :+: Lf "c"))       ==  ["a","b","c"] &&
  leaves ((Lf "a" :+: Lf "b") :+: Lf "c")       ==  ["a","b","c"] 

-- 3c

shift :: Tree a -> Tree a
shift (Lf a) = Lf a
shift (Lf a :+: c) = Lf a :+: shift c
shift ((a :+: b) :+: c) = shift (a :+: (b :+: c))

prop_tree :: Eq a => Tree a -> Bool
prop_tree xt  =  right (shift xt) && leaves xt == leaves (shift xt)

test3c = 
  shift (Lf 1)
     ==  (Lf 1) &&
  shift (Lf 1 :+: (Lf 2 :+: (Lf 3 :+: Lf 4))) 
     == (Lf 1:+:(Lf 2:+:(Lf 3:+:Lf 4))) &&
  shift ((Lf 1 :+: Lf 2) :+: (Lf 3 :+: Lf 4)) 
     == (Lf 1:+:(Lf 2:+:(Lf 3:+:Lf 4))) &&
   shift (Lf "a" :+: (Lf "b" :+: Lf "c"))
     ==  (Lf "a" :+: (Lf "b" :+: Lf "c")) &&
   shift ((Lf "a" :+: Lf "b") :+: Lf "c")
     ==  (Lf "a" :+: (Lf "b" :+: Lf "c"))
