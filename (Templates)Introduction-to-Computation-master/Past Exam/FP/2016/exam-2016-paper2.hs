-- Informatics 1 Functional Programming
-- December 2016
-- SITTING 2 (14:30 - 16:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

f :: [Int] -> String -> Int
f a b = product [ x | (x,y) <- zip a b, y `elem` "y"]

test1a =
  f [3,5,2,4,1] "yynyn"     == 60 &&
  f [10,20,30,40,50] "abcd" == 1 && 
  f [] "baby"               == 1 &&
  f [4,3,2,1] "aye"         == 3

-- 1b

g :: [Int] -> String -> Int
g [] _  = 1
g _ []  = 1
g (x:xs) (y:ys) 
    | y == 'y'  = x * (g xs ys)
    | otherwise = g xs ys

test1b =
      g [3,5,2,4,1] "yynyn"     == 60 &&
      g [10,20,30,40,50] "abcd" == 1 && 
      g [] "baby"               == 1 &&
      g [4,3,2,1] "aye"         == 3   

prop1 :: [Int] -> String -> Bool
prop1 x y = f x y == g x y

-- Question 2

-- 2a

p :: String -> Int
p str = sum [ if even (digitToInt x) then digitToInt x else 0 | x <- str, isDigit x]

test2a = 
  p "Functional" == 0 &&
  p "3.157/3 > 19" == 0 &&
  p "42+12=54" == 12 &&
  p "1234567890" == 20

-- 2b

q :: String -> Int
q []  = 0
q (x:xs)
  | isDigit x && even (digitToInt x) = digitToInt x + q xs
  | otherwise = q xs

test2b = 
  q "Functional" == 0 &&
  q "3.157/3 > 19" == 0 &&
  q "42+12=54" == 12 &&
  q "1234567890" == 20

-- 2c

r :: String -> Int
r str =  foldr (+) 0 (filter (even) (map (digitToInt) (filter (isDigit) str)))

test2c = 
  r "Functional" == 0 &&
  r "3.157/3 > 19" == 0 &&
  r "42+12=54" == 12 &&
  r "1234567890" == 20

prop2 :: String -> Bool
prop2 x = (p x == q x) && (q x == r x)  
-- Question 3

data Move =
     Go Int            -- move the given distance in the current direction
   | Turn              -- reverse direction
   | Dance             -- dance in place, without changing direction
  deriving (Eq,Show)   -- defines obvious == and show

data Command =
     Nil                      -- do nothing
   | Command :#: Move         -- do a command followed by a move
  deriving Eq                 -- defines obvious ==

instance Show Command where   -- defines show :: Command -> String
  show Nil = "Nil"
  show (com :#: mov) = show com ++ " :#: " ++ show mov

type Position = Int
data Direction = L | R
  deriving (Eq,Show)          -- defines obvious == and show
type State = (Position, Direction)

-- For QuickCheck

instance Arbitrary Move where
  arbitrary = sized expr
    where
      expr n | n <= 0 = elements [Turn, Dance]
             | otherwise = liftM (Go) arbitrary

instance Arbitrary Command where
  arbitrary = sized expr
    where
      expr n | n <= 0 = oneof [elements [Nil]]
             | otherwise = oneof [ liftM2 (:#:) subform arbitrary
                                 ]
             where
               subform = expr (n-1)

instance Arbitrary Direction where
  arbitrary = elements [L,R]

-- 3a

state :: Move -> State -> State
state Dance a = a
state (Go a) (b,R) = (a+b,R)
state (Go a) (b,L) = (b-a,L)
state Turn (a,R) = (a,L)
state Turn (a,L) = (a,R)

-- 3b

trace :: Command -> State -> [State]
trace (Nil) c = [c]
trace (a :#: b) c = trace a c ++ [state b (last (trace a c))]

test3b = 
  trace (Nil) (3,R) == [(3,R)] &&
  trace (Nil :#: Go 3 :#: Turn :#: Go 4) (0,L)
    == [(0,L),(-3,L),(-3,R),(1,R)] &&
  trace (Nil :#: Go 3 :#: Turn :#: Dance :#: Turn) (0,R)
                    == [(0,R),(3,R),(3,L),(3,L),(3,R)] &&
  trace (Nil :#: Go 3 :#: Turn :#: Go 2 :#: Go 1 :#: Turn :#: Go 4) (4,L)
                       == [(4,L),(1,L),(1,R),(3,R),(4,R),(4,L),(0,L)]
-- 3c

--samepos :: State -> [State] -> Bool
--samepos (a,b) ss = a `elem` (map fst ss)

furtherpos :: State -> [State] -> Bool
furtherpos (p,s) ss = and [ abs p > abs q | (q,_) <- ss ]

dancify :: Command -> Command
dancify Nil = Nil
dancify (a :#: Dance) = (dancify a) :#: Dance
dancify (a :#: b)
        | furtherpos (state b (last (trace a (0,R)))) (trace a (0,R)) = (dancify a) :#: b :#: Dance
        | otherwise = (dancify a) :#: b

test3c = 
  dancify Nil
         == Nil &&
  dancify (Nil :#: Go 3 :#: Turn :#: Go 4)
         == Nil :#: Go 3 :#: Dance :#: Turn :#: Go 4 &&
  dancify (Nil :#: Go 3 :#: Turn :#: Dance :#: Turn)
         == Nil :#: Go 3 :#: Dance :#: Turn :#: Dance :#: Turn &&
  dancify (Nil :#: Go 3 :#: Turn :#: Go 2 :#: Go 1 :#: Turn :#: Go 4)
         == Nil :#: Go 3 :#: Dance :#: Turn :#: Go 2 :#: Go 1
                                            :#: Turn :#: Go 4 :#: Dance

