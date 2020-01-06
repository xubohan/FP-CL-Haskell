module OrderedListUnabsTest where
import OrderedListUnabs

test :: Int -> Bool
test n = 
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- Prelude OrderedListUnabsTest> test 100000
-- True
-- (83.98 secs, 107886888 bytes)

badtest :: Int -> Bool
badtest n = 
  s `equal` t
  where
  s = [1,2..n]     -- no call to set!
  t = [n,n-1..1]   -- no call to set!
