module OrderedListUnabs
  (Set,empty,insert,set,element,equal,check) where

import Data.List(nub,sort)
import Test.QuickCheck

type Set a = [a]

invariant :: Ord a => Set a -> Bool
invariant xs  =
  and [ x < y | (x,y) <- zip xs (tail xs) ]

empty :: Set a
empty =  []

insert :: Ord a => a -> Set a -> Set a
insert x []               =  [x]
insert x (y:ys) | x < y   =  x : y : ys
                | x == y  =  y : ys
                | x > y   =  y : insert x ys

set :: Ord a => [a] -> Set a
set xs  =  nub (sort xs)

element :: Ord a => a -> Set a -> Bool
x `element` []                =  False
x `element` (y:ys) | x < y    =  False
                   | x == y   =  True
                   | x > y    =  x `element` ys

equal :: Eq a => Set a -> Set a -> Bool
xs `equal` ys  =  xs == ys

prop_invariant :: [Int] -> Bool
prop_invariant xs  =  invariant s
  where
  s = set xs

prop_element :: [Int] -> Bool
prop_element ys  =
  and [ x `element` s == odd x | x <- ys ]
  where
  s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_invariant >>
  quickCheck prop_element

-- Prelude OrderedListUnabs> check
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
