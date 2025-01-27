module BalancedTreeUnabsTest where
import BalancedTreeUnabs

test :: Int -> Bool
test n = 
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- Prelude BalancedTreeUnabsTest> test 100000
-- True
-- (6.03 secs, 1389813752 bytes)

badtest :: Bool
badtest =
  s `equal` t
  where
  s = set [1,2,3]
  t = (Node Nil 1 (Node Nil 2 (Node Nil 3 Nil 1) 2) 3)
  -- breaks the invariant!
