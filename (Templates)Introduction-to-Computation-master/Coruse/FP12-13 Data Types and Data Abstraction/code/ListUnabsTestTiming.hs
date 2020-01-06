module ListUnabsTest where
import ListUnabs

test :: Int -> Bool
test n = 
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- Prelude ListUnabsTest> test 100000
-- True
-- (81.40 secs, 20932356 bytes)

breakAbstraction :: Set a -> a
breakAbstraction =  head

-- not a function!
-- head (set [1,2,3]) == 1 /= 3 == head (set [3,2,1])
