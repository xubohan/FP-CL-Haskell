module TreeUnabsTest where
import TreeUnabs

test :: Int -> Bool
test n = 
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- Prelude TreeUnabsTest> test 100000
-- True
-- (4623.23 secs, 957412683436 bytes)

badtest :: Bool
badtest =
  s `equal` t
  where
  s = set [1,2,3]
  t = Node (Node Nil 3 Nil) 2 (Node Nil 1 Nil)
  -- breaks the invariant!
