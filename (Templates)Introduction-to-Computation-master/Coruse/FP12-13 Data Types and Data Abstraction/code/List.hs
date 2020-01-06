module List
  (Set,empty,insert,set,element,equal,check) where
import Test.QuickCheck

type Set a = [a]

empty :: Set a
empty =  []

insert :: a -> Set a -> Set a
insert x xs  =  x:xs

set :: [a] -> Set a
set xs  =  xs

element :: Eq a => a -> Set a -> Bool
x `element` xs  =  x `elem` xs

equal :: Eq a => Set a -> Set a -> Bool
xs `equal` ys  =  xs `subset` ys && ys `subset` xs
  where
  xs `subset` ys  =  and [ x `elem` ys | x <- xs ]

prop_element :: [Int] -> Bool
prop_element ys  =
  and [ x `element` s == odd x | x <- ys ]
  where
  s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_element
-- Prelude List> check
-- +++ OK, passed 100 tests.
