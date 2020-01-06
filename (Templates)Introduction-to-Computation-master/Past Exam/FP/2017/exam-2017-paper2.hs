-- Informatics 1 Functional Programming
-- December 2017
-- SITTING 2 (14:30 - 16:30)

module Dec2017 where

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ), Gen, suchThat,
                        oneof, elements, sized, (==>) )
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

-- Question 1

f :: [Int] -> [String]
f = undefined
 
g :: [Int] -> [String]
g = undefined

-- Question 2

-- 2a

isInitialism :: String -> Bool
isInitialism = undefined

p :: [String] -> Int
p = undefined

-- 2b

isInitialism' :: String -> Bool
isInitialism' = undefined

q :: [String] -> Int
q = undefined

-- 2c

r :: [String] -> Int
r = undefined

-- Question 3

data Expr = X                      -- variable
          | Const Int              -- integer constant >=0
          | Expr :+: Expr          -- addition
          | Expr :*: Expr          -- multiplication
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [ return X
                                       , liftM Const genPos ]
                 | otherwise  =  oneof [ return X
                                       , liftM Const genPos
                                       , liftM2 (:+:) subform2 subform2
                                       , liftM2 (:*:) subform2 subform2
                                       ]
                 where
                   subform2  =  expr (n `div` 2)
                   genPos  =  oneof [ return 0, return 1, return 2, return 3, return 4,
                                      return 5, return 6, return 7, return 8, return 8 ]

-- 3a

eval :: Expr -> Int -> Int
eval X a = a
eval (Const a) _ = a
eval (p :+: q) a = (eval p a) + (eval q a)
eval (p :*: q) a = (eval p a) * (eval q a)
-- 3b

isSimple :: Expr -> Bool
isSimple X = True
isSimple (Const _) = True
isSimple (Const a :*: q) = False
isSimple (p :+: q) = (isSimple p) && (isSimple q)
isSimple (p :*: q) = (isSimple p) && (isSimple q)

test3b =
  isSimple ((X :*: Const 3) :+: (Const 0 :*: X)) == False &&
  isSimple (X :*: (Const 3 :+: Const 4))         == True &&
  isSimple (Const 4 :+: (Const 3 :*: X))         == False &&
  isSimple (((Const 1 :*: Const 2) :*: (X :+: Const 1)) :*: Const 2) == False

-- 3c

simplify :: Expr -> Expr
simplify = undefined
