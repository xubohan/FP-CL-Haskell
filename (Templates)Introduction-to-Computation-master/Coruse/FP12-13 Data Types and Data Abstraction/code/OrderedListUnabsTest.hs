module OrderedListUnabsTest where
import OrderedListUnabs

test :: Int -> Bool
test n = 
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

breakAbstraction :: Set a -> a
breakAbstraction =  head
-- now it's a function
-- head (set [1,2,3]) == 1 == head (set [3,2,1])

badtest :: Int -> Bool
badtest n = 
  s `equal` t
  where
  s = [1,2..n]     -- no call to set!
  t = [n,n-1..1]   -- no call to set!
