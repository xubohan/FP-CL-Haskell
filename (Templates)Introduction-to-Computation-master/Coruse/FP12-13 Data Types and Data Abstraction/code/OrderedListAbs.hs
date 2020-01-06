module OrderedListAbs
  (Set,empty,insert,set,element,equal,check) where
import Data.List(nub,sort)
import Test.QuickCheck

data Set a = MkSet [a]

invariant :: Ord a => Set a -> Bool
invariant (MkSet xs)  =
  and [ x < y | (x,y) <- zip xs (tail xs) ]

empty :: Set a
empty =  MkSet []

insert :: Ord a => a -> Set a -> Set a
insert x (MkSet ys)  =  MkSet (ins x ys)
  where
  ins x []               =  [x]
  ins x (y:ys) | x < y   =  x : y : ys
               | x == y  =  y : ys
               | x > y   =  y : ins x ys

set :: Ord a => [a] -> Set a
set xs  =  MkSet (nub (sort xs))

element :: Ord a => a -> Set a -> Bool
x `element` MkSet ys  =  x `elt` ys
  where
  x `elt` []                =  False
  x `elt` (y:ys) | x < y    =  False
                 | x == y   =  True
                 | x > y    =  x `elt` ys

equal :: Eq a => Set a -> Set a -> Bool
MkSet xs `equal` MkSet ys  =  xs == ys

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

-- Prelude OrderedListAbs> check
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
