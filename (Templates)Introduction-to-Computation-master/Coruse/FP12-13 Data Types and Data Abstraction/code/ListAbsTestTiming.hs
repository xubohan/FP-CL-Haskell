module ListAbsTest where
import ListAbs

test :: Int -> Bool
test n = 
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- Prelude ListAbsTest> test 100000
-- True
-- (80.96 secs, 18886896 bytes)

-- Following no longer type checks!
-- breakAbstraction :: Set a -> a
-- breakAbstraction =  head
