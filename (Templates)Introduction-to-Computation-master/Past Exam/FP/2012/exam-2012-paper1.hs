-- Informatics 1 Functional Programming
-- December 2012
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below

-- Question 1

-- 1a

f :: Int -> [Int] -> [Int]
f a [] = []
f a [x] = [a]
f a str = [ if even x then y else a | (x,y) <- zip [1..] str]

test1a = 
  f 0 [1,2,3,4,5] == [0,2,0,4,0] &&
  f 0 [1,2,3,4] == [0,2,0,4] &&
  f 0 [] == [] &&
  f 0 [7] == [0]

-- 1b

g :: Int -> [Int] -> [Int]
g a [] = []
g a [x] = [a]
g a (x:y:z) = [a]++[y] ++ g a z

test1b = 
  g 0 [1,2,3,4,5] == [0,2,0,4,0] &&
  g 0 [1,2,3,4] == [0,2,0,4] &&
  g 0 [] == [] &&
  g 0 [7] == [0]

--prop_check x s = f x s == g x s

-- Question 2

-- 2a

p :: [Int] -> Bool
p str = and [ even x | x <- str, x <= 100, x >= 10]

test2a =
  p [1,12,153,84,64,9] == True &&
  p [1,12,153,83,9] == False &&
  p [] == True &&
  p [1,151] == True

-- 2b

q :: [Int] -> Bool
q [] = True
q (x:xs) 
  | x >= 10 &&
    x <= 100    = even x && q xs
  | otherwise   = q xs

test2b =
    q [1,12,153,84,64,9] == True &&
    q [1,12,153,83,9] == False &&
    q [] == True &&
    q [1,151] == True
-- 2c

r :: [Int] -> Bool
r str = and $  map even (filter (\x -> x >= 10 && x <= 100) str)

test2c =
  r [1,12,153,84,64,9] == True &&
  r [1,12,153,83,9] == False &&
  r [] == True &&
  r [1,151] == True
-- Question 3

data Prop = X
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          deriving (Eq, Ord)

-- turns a Prop into a string approximating mathematical notation

showProp :: Prop -> String
showProp X          =  "X"
showProp F          =  "F"
showProp T          =  "T"
showProp (Not p)    =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)  =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"

-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       ]
                 where
                   atom = oneof [elements [X,F,T]]
                   subform  =  prop (n `div` 2)

-- 3a

eval :: Prop -> Bool -> Bool
eval X v = v
eval F a = False 
eval T y = y
eval (Not a) z = not (eval a z)
eval (q :|: p) b = (eval q b) || (eval p b)

test3a = 
  eval (Not T) True == False &&
  eval (Not X) False == True &&
  eval (Not X :|: Not (Not X)) True == True &&
  eval (Not X :|: Not (Not X)) False == True &&
  eval (Not (Not X :|: F)) True == True &&
  eval (Not (Not X :|: F)) False == False

-- 3b

simplify :: Prop -> Prop
simplify X = X
simplify F = F
simplify T = T
simplify (Not p) = negate (simplify p)
  where
    negate T = F
    negate F = T
    negate (Not p) = p
    negate p = Not p
simplify (p :|: q) = disjoin (simplify p) (simplify q)
    where
      disjoin T p = T
      disjoin F p = p
      disjoin p T = T
      disjoin p F = p
      disjoin p q
          | p == q  = p
          | otherwise = p :|: q

test3b =
          simplify (Not X :|: Not (Not X)) == Not X :|: X
          && simplify (Not (Not X :|: F)) == X
          && simplify (Not T) == F
          && simplify (Not F :|: X) == T
          && simplify (Not (Not (Not X) :|: X)) == Not X

test3 = test3a && test3b
prop_3 p =
  eval p True == eval (simplify p) True
  && eval p False == eval (simplify p) False
  && length (showProp p) >= length (showProp (simplify p))
check3 = quickCheck prop_3

