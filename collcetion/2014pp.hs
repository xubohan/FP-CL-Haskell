import Data.Char
import Test.QuickCheck

f :: Char -> Int
f x
  | elem x "haskell" = 5
  | elem x "HASKELL" = 10
  | isLower x        = 1
  | isUpper x        = 2
  | otherwise        = 0

g :: String -> Int
g str = product[f x| x <- str, isAlpha x]

h :: String -> Int
h [ ] = 1
h (x:xs)
        | isAlpha x     = (f x) * h xs
        | otherwise     = h xs

prop_check :: String -> Bool
prop_check x = g x == h x

test_1 = quickCheck prop_check
------------------------------
c :: String -> String -> String
c x y = [ a | (a,b) <- zip x y, a == b]

d :: String -> String -> String
d [ ] y = [ ]
d x [ ] = [ ]
d (x:xs) (y:ys)
              | x == y  = x : d xs ys
              | otherwise = d xs ys

prop_cd :: String -> String -> Bool
prop_cd x y = c x y == d x y
test_2 = quickCheck prop_cd

matrix :: Int -> Int -> [String]
matrix a b = [ show (i*(-1)^(i+j)) ++ "/"++show (j)| i <- [1..a], j <- [1..b]]