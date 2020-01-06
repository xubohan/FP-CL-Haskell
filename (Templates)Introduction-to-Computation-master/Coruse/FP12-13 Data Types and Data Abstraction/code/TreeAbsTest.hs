module TreeAbsTest where
import TreeAbs

test :: Int -> Bool
test n = 
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- Following no longer compiles!
-- Not in scope: data constructor `Node'
-- Not in scope: data constructor `Nil'
-- badtest :: Bool
-- badtest =
--   s `equal` t
--   where
--   s = set [1,2,3]
--   t = Node (Node Nil 3 Nil) 2 (Node Nil 1 Nil)
--   -- breaks the invariant!
