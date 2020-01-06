module BalancedTreeAbsTest where
import BalancedTreeAbs

test :: Int -> Bool
test n = 
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- Prelude BalancedTreeAbsTest> test 100000
-- True
-- (6.33 secs, 1395538624 bytes)

-- Following no longer compiles!
-- Not in scope: data constructor `Node'
-- Not in scope: data constructor `Nil'
-- badtest :: Bool
-- badtest =
--   s `equal` t
--   where
--   s = set [1,2,3]
--   t = (Node Nil 1 (Node Nil 2 (Node Nil 3 Nil 1) 2) 3)
--   -- breaks the invariant!
