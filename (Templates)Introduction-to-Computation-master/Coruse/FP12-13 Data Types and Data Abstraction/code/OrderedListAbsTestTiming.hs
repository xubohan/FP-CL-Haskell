module OrderedListAbsTest where
import OrderedListAbs

test :: Int -> Bool
test n = 
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- Prelude OrderedListAbsTest> test 100000
-- True
-- (86.11 secs, 102234716 bytes)

-- Following no longer type checks!
-- badtest :: Int -> Bool
-- badtest n = 
--   s `equal` t
--   where
--   s = [1,2..n]     -- no call to set!
--   t = [n,n-1..1]   -- no call to set!